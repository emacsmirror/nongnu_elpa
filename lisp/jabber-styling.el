;;; jabber-styling.el --- XEP-0393 Message Styling  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Maintainer: Thanos Apollo <public@thanosapollo.org>

;; This file is a part of jabber.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; XEP-0393 Message Styling (v1.1.1).
;; Applies formatted text styling to chat message bodies: *bold*,
;; _italic_, ~strikethrough~, `preformatted`, ```code blocks```, and
;; > block quotes.
;;
;; Code blocks whose opening fence carries a language token
;; (```lang) are fontified with that language's major mode, so the
;; user's theme applies.  The token has no meaning per XEP-0393.
;;
;; Message display area: a post-body printer in `jabber-chat-printers'
;; applies styling after `jabber-chat-print-body' inserts text.
;;
;; Composition area: jit-lock provides live styling preview as the
;; user types.

;;; Code:

(require 'jabber-disco)

(require 'jit-lock)

(require 'org-faces)

(eval-when-compile (require 'cl-lib))

(defgroup jabber-styling nil
  "XEP-0393 Message Styling options."
  :group 'jabber-chat)

(defcustom jabber-styling-enable t
  "Whether to render XEP-0393 Message Styling in chat buffers."
  :type 'boolean)

(defcustom jabber-styling-fontify-code-blocks t
  "Whether to fontify ```lang code blocks with the language's major mode.
When nil, or when no major mode matches the language token, the
code between the fences still gets the block background but no
native syntax highlighting."
  :type 'boolean)

(defcustom jabber-styling-code-lang-modes
  '(("elisp" . emacs-lisp-mode) ("el" . emacs-lisp-mode)
    ("shell" . sh-mode) ("bash" . sh-mode) ("sh" . sh-mode)
    ("cpp" . c++-mode) ("c++" . c++-mode)
    ("js" . js-mode))
  "Alist mapping fence language tokens to major modes.
Languages not listed here resolve as LANG-mode via `intern-soft'."
  :type '(alist :key-type (string :tag "Language")
                :value-type (function :tag "Major mode")))

(defcustom jabber-styling-fontify-max-size 10000
  "Maximum code block size in characters to fontify natively.
Larger blocks still get the block background but no native syntax
highlighting."
  :type 'natnum)

(defconst jabber-styling-xmlns "urn:xmpp:styling:0"
  "XEP-0393 Message Styling namespace.")

;;; Faces

(defface jabber-styling-bold '((t :inherit bold))
  "Face for *bold* spans.")

(defface jabber-styling-italic '((t :inherit italic))
  "Face for _italic_ spans.")

(defface jabber-styling-strike '((t :strike-through t))
  "Face for ~strikethrough~ spans.")

(defface jabber-styling-pre '((t :inherit font-lock-constant-face))
  "Face for `preformatted` inline spans.")

(defface jabber-styling-pre-block '((t :inherit org-block :extend t))
  "Background face for the body of a ```code block```.
Appended beneath the language mode's own font-lock faces, exactly
as `org-src-font-lock-fontify-block' applies `org-block': being a
background face, it tints the block without overriding the code's
foreground colors.  Inherits `org-block' so blocks match the
user's Org theme.")

(defface jabber-styling-pre-block-fence '((t :inherit org-block-begin-line :extend t))
  "Face for the ``` fence lines delimiting a code block.
Inherits `org-block-begin-line', mirroring Org's block delimiters.")

(defface jabber-styling-quote '((t :inherit shadow))
  "Face for > block quotes.")

;;; Span regexes
;;
;; Each regex matches: DIRECTIVE CONTENT DIRECTIVE
;; where content starts/ends with non-whitespace and does not contain
;; the directive char.  Group 1 captures content without delimiters.
;; The [^D]* quantifier gives lazy semantics naturally since it
;; cannot consume the closing delimiter.

(defconst jabber-styling--bold-re
  "\\*\\([^[:space:]*]\\(?:[^*]*[^[:space:]*]\\)?\\)\\*"
  "Regex for *bold* spans.  Group 1 is content.")

(defconst jabber-styling--italic-re
  "_\\([^[:space:]_]\\(?:[^_]*[^[:space:]_]\\)?\\)_"
  "Regex for _italic_ spans.  Group 1 is content.")

(defconst jabber-styling--strike-re
  "~\\([^[:space:]~]\\(?:[^~]*[^[:space:]~]\\)?\\)~"
  "Regex for ~strikethrough~ spans.  Group 1 is content.")

(defconst jabber-styling--pre-re
  "`\\([^[:space:]`]\\(?:[^`]*[^[:space:]`]\\)?\\)`"
  "Regex for `preformatted` spans.  Group 1 is content.")

;;; Pure parsing functions

(defun jabber-styling--valid-opening-p (str pos)
  "Non-nil if POS in STR is a valid XEP-0393 opening position.
Opening must be at start, after whitespace, or after another
opening directive."
  (or (zerop pos)
      (memq (aref str (1- pos)) '(?\s ?\t ?\n ?* ?_ ?~ ?`))))

(defun jabber-styling--in-region-p (pos regions)
  "Non-nil if POS falls inside any interval in REGIONS.
REGIONS is a list of (START . END) cons cells."
  (cl-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r))))
           regions))

(defun jabber-styling--match-spans (str re face pre-regions)
  "Match span RE in STR, returning (START END FACE) triples.
Skips matches whose opening falls inside PRE-REGIONS."
  (let ((spans nil)
        (pos 0))
    (while (string-match re str pos)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (if (and (jabber-styling--valid-opening-p str beg)
                 (not (jabber-styling--in-region-p beg pre-regions)))
            (progn
              (push (list beg end face) spans)
              (setq pos end))
          (setq pos (1+ beg)))))
    (nreverse spans)))

(defun jabber-styling--parse-spans (line)
  "Parse XEP-0393 span directives in LINE.
Return a list of (START END FACE) triples for styled regions.
Preformatted spans are matched first and suppress inner
directives."
  (let ((pre-regions nil)
        (spans nil))
    ;; First pass: backtick spans (suppress inner directives)
    (let ((pos 0))
      (while (string-match jabber-styling--pre-re line pos)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (if (jabber-styling--valid-opening-p line beg)
              (progn
                (push (list beg end 'jabber-styling-pre) spans)
                (push (cons beg end) pre-regions)
                (setq pos end))
            (setq pos (1+ beg))))))
    ;; Second pass: other spans, skipping pre regions
    (dolist (pair `((,jabber-styling--bold-re . jabber-styling-bold)
                    (,jabber-styling--italic-re . jabber-styling-italic)
                    (,jabber-styling--strike-re . jabber-styling-strike)))
      (setq spans (nconc spans
                         (jabber-styling--match-spans
                          line (car pair) (cdr pair) pre-regions))))
    (sort spans (lambda (a b) (< (car a) (car b))))))

(defun jabber-styling--classify-block (line)
  "Classify LINE as a block type.
Return one of: `pre-open', `pre-close', `quote', or `plain'.
Pre-open matches lines beginning with ```.  Pre-close matches
lines containing only ```."
  (cond
   ((string-match-p "\\`\n*\\'" line) 'plain)
   ((string-match-p "\\````\\'" line) 'pre-close)
   ((string-match-p "\\````" line) 'pre-open)
   ((string-match-p "\\`>" line) 'quote)
   (t 'plain)))

(defun jabber-styling--parse-blocks (text)
  "Parse TEXT into XEP-0393 blocks.
Return a list of (TYPE START END) triples where TYPE is one of
`plain', `quote', or `pre'."
  (let ((blocks nil)
        (len (length text))
        (offset 0)
        (in-pre nil)
        (pre-start nil))
    (while (< offset len)
      (let* ((nl (or (cl-position ?\n text :start offset) len))
             (line (substring text offset nl))
             (line-end (min (1+ nl) len))
             (kind (jabber-styling--classify-block line)))
        (cond
         (in-pre
          (when (eq kind 'pre-close)
            (push (list 'pre pre-start line-end) blocks)
            (setq in-pre nil)))
         ((memq kind '(pre-open pre-close))
          (setq in-pre t
                pre-start offset))
         ((eq kind 'quote)
          (push (list 'quote offset line-end) blocks))
         (t
          (push (list 'plain offset line-end) blocks)))
        (setq offset line-end)))
    (when in-pre
      (push (list 'pre pre-start len) blocks))
    (nreverse blocks)))

(defun jabber-styling--fence-lang (line)
  "Extract the language token from opening fence LINE.
Return the downcased first whitespace-delimited token after ```,
or nil if absent."
  (and (string-match "\\````\\([^ \t\n]+\\)" line)
       (downcase (match-string 1 line))))

(defun jabber-styling--pre-block-parts (text)
  "Decompose pre block TEXT into (LANG CODE-START CODE-END).
TEXT is one block as delimited by `jabber-styling--parse-blocks'.
LANG is the fence language token or nil.  CODE-START and CODE-END
are offsets into TEXT delimiting the code lines, excluding both
fence lines.  Unterminated blocks yield CODE-END = length of TEXT."
  (let* ((len (length text))
         (first-nl (cl-position ?\n text))
         (code-start (if first-nl (1+ first-nl) len))
         (body (if (and (> len 0) (eq (aref text (1- len)) ?\n))
                   (substring text 0 (1- len))
                 text))
         (last-start (let ((nl (cl-position ?\n body :from-end t)))
                       (if nl (1+ nl) 0)))
         (code-end (if (eq (jabber-styling--classify-block
                            (substring body last-start))
                           'pre-close)
                       (max code-start last-start)
                     len)))
    (list (jabber-styling--fence-lang (substring text 0 (or first-nl len)))
          code-start code-end)))

(defun jabber-styling--lang-mode (lang)
  "Resolve LANG to a major mode function, or nil.
Consults `jabber-styling-code-lang-modes', then LANG-mode via
`intern-soft', remaps through `major-mode-remap-alist', and
requires the result to be `fboundp'."
  (and-let* ((lang)
             (mode (or (cdr (assoc lang jabber-styling-code-lang-modes))
                       (intern-soft (concat lang "-mode"))))
             (mode (alist-get mode major-mode-remap-alist mode))
             ((fboundp mode)))
    mode))

;;; Code block fontification

(defun jabber-styling--fontification-buffer (mode)
  "Return the persistent hidden fontification buffer for MODE.
Created on first use."
  (get-buffer-create (format " *jabber-styling-fontify:%s*" mode)))

(defun jabber-styling--face-stretches (limit)
  "Collect (START END FACE) triples from the current buffer.
START and END are 0-based offsets clamped to LIMIT.  FACE is the
non-nil face covering that stretch."
  (let ((pos (point-min))
        (stretches nil))
    (while (< (1- pos) limit)
      (let ((next (or (next-property-change pos) (point-max)))
            ;; Some modes apply font-lock-face instead of face in
            ;; buffers where font-lock-mode is off (the org-src gotcha).
            (face (or (get-text-property pos 'face)
                      (get-text-property pos 'font-lock-face))))
        (when face
          (push (list (1- pos) (min (1- next) limit) face) stretches))
        (setq pos next)))
    (nreverse stretches)))

(defun jabber-styling--fontify-code (code mode)
  "Fontify CODE string with major mode MODE.
Return a list of (START END FACE) triples with 0-based offsets
into CODE, computed in a hidden work buffer.  Return nil if MODE
fails to initialize.

Like `org-src-font-lock-fontify-block', MODE is run with its full
hooks, so minor modes the user enables for that language --
`rainbow-delimiters-mode' and the like -- also fontify the block.
The buffer is reused per MODE, so the hooks run once."
  (with-current-buffer (jabber-styling--fontification-buffer mode)
    (erase-buffer)
    ;; Trailing space guarantees a final property change boundary.
    (insert code " ")
    ;; Guard the whole path: running a mode's full hooks and its
    ;; font-lock keywords can signal, and this feeds the chat printer.
    (condition-case err
        (progn
          (unless (eq major-mode mode)
            (funcall mode))
          (font-lock-ensure)
          (jabber-styling--face-stretches (length code)))
      (error
       (message "jabber-styling: %s fontification failed: %s"
                mode (error-message-string err))
       nil))))

;;; Application

(defun jabber-styling--apply-spans (start line)
  "Apply span styling to LINE inserted at buffer position START."
  (dolist (span (jabber-styling--parse-spans line))
    (let ((sstart (+ start (nth 0 span)))
          (send (+ start (nth 1 span)))
          (face (nth 2 span)))
      (font-lock-prepend-text-property sstart send 'face face))))

(defun jabber-styling--strip-quote-prefix (line)
  "Strip the leading > and first whitespace char from LINE.
Per XEP-0393, the first leading whitespace after > MUST be trimmed."
  (if (and (> (length line) 1) (eq (aref line 0) ?>))
      (if (memq (aref line 1) '(?\s ?\t))
          (substring line 2)
        (substring line 1))
    (if (and (= (length line) 1) (eq (aref line 0) ?>))
        ""
      line)))

(defun jabber-styling--apply-code-block (cbeg cend lang)
  "Natively fontify the code region between CBEG and CEND for LANG.
Resolves LANG to a major mode; when found and the region fits
`jabber-styling-fontify-max-size', sets the mode's font-lock faces
exactly, as `org-src-font-lock-fontify-block' does, and marks the
region with the `jabber-styling-fontified' text property.  Return
non-nil when faces were applied.  The caller clears the region
first and supplies the block backdrop; this only sets mode faces."
  (let ((mode (jabber-styling--lang-mode lang)))
    (when (and mode
               (<= (- cend cbeg) jabber-styling-fontify-max-size))
      (let* ((code (buffer-substring-no-properties cbeg cend))
             (stretches (jabber-styling--fontify-code code mode)))
        (when stretches
          (pcase-dolist (`(,s ,e ,face) stretches)
            (put-text-property (+ cbeg s) (+ cbeg e) 'face face))
          (put-text-property cbeg cend 'jabber-styling-fontified t)
          t)))))

(defun jabber-styling--apply-region (start end)
  "Apply XEP-0393 styling to text between START and END in current buffer."
  (let ((text (buffer-substring-no-properties start end)))
    (dolist (block (jabber-styling--parse-blocks text))
      (let ((type (nth 0 block))
            (bstart (+ start (nth 1 block)))
            (bend (min (+ start (nth 2 block)) end)))
        (pcase type
          ('pre
           ;; org-src model: the ``` fence lines get the delimiter
           ;; face; the body is fontified by its language mode with
           ;; the block background appended beneath, so the tint never
           ;; overrides the mode's foreground colors.
           (pcase-let* ((`(,lang ,code-start ,code-end)
                         (jabber-styling--pre-block-parts
                          (buffer-substring-no-properties bstart bend)))
                        (cbeg (+ bstart code-start))
                        (cend (min (+ bstart code-end) bend)))
             (when (< bstart cbeg)
               (font-lock-prepend-text-property
                bstart cbeg 'face 'jabber-styling-pre-block-fence))
             (when (< cend bend)
               (font-lock-prepend-text-property
                cend bend 'face 'jabber-styling-pre-block-fence))
             (when (< cbeg cend)
               ;; When fontifying, clear the body to a clean slate
               ;; first (org-src style) so every enabled block renders
               ;; uniformly, whether or not the language resolves to a
               ;; mode -- rather than only recognized languages losing
               ;; the surrounding chat face.
               (when jabber-styling-fontify-code-blocks
                 (remove-text-properties cbeg cend '(face nil))
                 (jabber-styling--apply-code-block cbeg cend lang))
               (font-lock-append-text-property
                cbeg cend 'face 'jabber-styling-pre-block))))
          ('quote
           (font-lock-prepend-text-property
            bstart bend 'face 'jabber-styling-quote)
           (let* ((line (buffer-substring-no-properties bstart bend))
                  (stripped (jabber-styling--strip-quote-prefix line))
                  (prefix-len (- (length line) (length stripped))))
             (jabber-styling--apply-spans (+ bstart prefix-len) stripped)))
          ('plain
           (let ((line (buffer-substring-no-properties bstart bend)))
             (jabber-styling--apply-spans bstart line))))))))

;;; Live styling (composition area)

(defconst jabber-styling--all-faces
  '(jabber-styling-bold jabber-styling-italic jabber-styling-strike
			jabber-styling-pre jabber-styling-pre-block
			jabber-styling-pre-block-fence jabber-styling-quote)
  "All faces applied by XEP-0393 styling.")

(defvar jabber-point-insert)            ; jabber-chatbuffer.el

(defun jabber-styling--remove-faces (beg end)
  "Remove XEP-0393 styling faces from BEG to END.
Preserves all other face properties in the region."
  (let ((pos beg))
    (while (< pos end)
      (let* ((next (or (next-single-property-change pos 'face nil end) end))
             (face (get-text-property pos 'face)))
        (when face
          (let ((new-face
                 (if (listp face)
                     (let ((filtered (cl-remove-if
                                      (lambda (f)
                                        (memq f jabber-styling--all-faces))
                                      face)))
                       (pcase (length filtered)
                         (0 nil)
                         (1 (car filtered))
                         (_ filtered)))
                   (unless (memq face jabber-styling--all-faces) face))))
            (unless (equal face new-face)
              (put-text-property pos next 'face new-face))))
        (setq pos next)))))

(defun jabber-styling--remove-code-fontification (beg end)
  "Remove natively applied code faces between BEG and END.
Clears the face property, plus the marker property, on stretches
carrying `jabber-styling-fontified'."
  (let ((pos beg))
    (while (setq pos (text-property-any pos end 'jabber-styling-fontified t))
      (let ((next (or (next-single-property-change
                       pos 'jabber-styling-fontified nil end)
                      end)))
        (remove-text-properties
         pos next '(face nil jabber-styling-fontified nil))
        (setq pos next)))))

(defun jabber-styling--fontify-compose (_beg end)
  "Apply XEP-0393 styling to the composition area.
Called by jit-lock for the region _BEG to END.  Only operates on
text after `jabber-point-insert' (the composition prompt).
Always refontifies the entire composition area to handle
multi-line constructs like pre blocks correctly."
  (when (and jabber-styling-enable
             (bound-and-true-p jabber-point-insert)
             (markerp jabber-point-insert))
    (let ((compose-beg (marker-position jabber-point-insert))
          (compose-end (point-max)))
      (when (and (< compose-beg compose-end)
                 (>= end compose-beg))
        (with-silent-modifications
          (jabber-styling--remove-code-fontification compose-beg compose-end)
          (jabber-styling--remove-faces compose-beg compose-end)
          (jabber-styling--apply-region compose-beg compose-end))))))

(defun jabber-styling--setup-buffer ()
  "Set up live XEP-0393 styling preview in the composition area.
Registers a jit-lock fontification function that applies styling
as the user types."
  (when jabber-styling-enable
    (jit-lock-register #'jabber-styling--fontify-compose t)))

(add-hook 'jabber-chat-mode-hook #'jabber-styling--setup-buffer)

;;; Chat printer integration

(defvar jabber-chat--body-start)        ; jabber-chat.el

(defun jabber-styling--post-body (msg _who mode)
  "Apply XEP-0393 styling to the body from MSG just inserted.
MODE must be :insert for styling to apply."
  (when (and (eq mode :insert)
             jabber-styling-enable
             (not (plist-get msg :unstyled)))
    (let ((start (or jabber-chat--body-start (point-min)))
          (end (point)))
      (when (< start end)
        (jabber-styling--apply-region start end)))))

(defvar jabber-chat-printers)
(add-hook 'jabber-chat-printers #'jabber-styling--post-body t)

;;; Disco

(jabber-disco-advertise-feature jabber-styling-xmlns)

(provide 'jabber-styling)
;;; jabber-styling.el ends here
