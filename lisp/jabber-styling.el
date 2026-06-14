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
;; Message display area: a post-body printer in `jabber-chat-printers'
;; applies styling after `jabber-chat-print-body' inserts text.
;;
;; Composition area: jit-lock provides live styling preview as the
;; user types.

;;; Code:

(require 'jabber-disco)

(require 'jit-lock)

(eval-when-compile (require 'cl-lib))

(defgroup jabber-styling nil
  "XEP-0393 Message Styling options."
  :group 'jabber-chat)

(defcustom jabber-styling-enable t
  "Whether to render XEP-0393 Message Styling in chat buffers."
  :type 'boolean)

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

(defface jabber-styling-pre-block '((t :inherit font-lock-constant-face))
  "Face for ```preformatted code blocks```.")

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

(defun jabber-styling--apply-region (start end)
  "Apply XEP-0393 styling to text between START and END in current buffer."
  (let ((text (buffer-substring-no-properties start end)))
    (dolist (block (jabber-styling--parse-blocks text))
      (let ((type (nth 0 block))
            (bstart (+ start (nth 1 block)))
            (bend (min (+ start (nth 2 block)) end)))
        (pcase type
          ('pre
           (font-lock-prepend-text-property
            bstart bend 'face 'jabber-styling-pre-block))
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
			jabber-styling-pre jabber-styling-pre-block jabber-styling-quote)
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
