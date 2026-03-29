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
;; Message display area: a body-printer replacement applies styling
;; after `jabber-chat-normal-body' inserts the text.
;;
;; Composition area: jit-lock provides live styling preview as the
;; user types, following the same approach as markdown-mode's
;; font-lock integration.

;;; Code:

(require 'jabber-disco)

(require 'jit-lock)

(eval-when-compile (require 'cl-lib))

(defgroup jabber-styling nil
  "XEP-0393 Message Styling options."
  :group 'jabber-chat)

(defcustom jabber-styling-enable t
  "Whether to render XEP-0393 Message Styling in chat buffers."
  :type 'boolean
  :group 'jabber-styling)

(defconst jabber-styling-xmlns "urn:xmpp:styling:0"
  "XEP-0393 Message Styling namespace.")

;;; Faces

(defface jabber-styling-bold '((t :inherit bold))
  "Face for *bold* spans."
  :group 'jabber-styling)

(defface jabber-styling-italic '((t :inherit italic))
  "Face for _italic_ spans."
  :group 'jabber-styling)

(defface jabber-styling-strike '((t :strike-through t))
  "Face for ~strikethrough~ spans."
  :group 'jabber-styling)

(defface jabber-styling-pre '((t :inherit font-lock-constant-face))
  "Face for `preformatted` inline spans."
  :group 'jabber-styling)

(defface jabber-styling-pre-block '((t :inherit font-lock-constant-face))
  "Face for ```preformatted code blocks```."
  :group 'jabber-styling)

(defface jabber-styling-quote '((t :inherit shadow))
  "Face for > block quotes."
  :group 'jabber-styling)

;;; Pure parsing functions

(defun jabber-styling--find-closing (line ch pos len)
  "Find the closing directive CH in LINE starting after POS.
LEN is the length of LINE.  Returns the position of the closing
directive, or nil.  The closing directive must not be preceded by
whitespace, and there must be at least one char between open and
close.  Matching is lazy (first valid close wins)."
  (let ((search (1+ pos)))
    (catch 'found
      (while (< search len)
        (when (and (eq (aref line search) ch)
                   (> search (1+ pos))
                   (not (memq (aref line (1- search))
                              '(?\s ?\t ?\n))))
          (throw 'found search))
        (setq search (1+ search)))
      nil)))

(defun jabber-styling--valid-opening-p (line pos directives)
  "Check if POS in LINE is a valid opening directive position.
DIRECTIVES is the alist of directive chars.  Opening must be at
start, after whitespace, or after another opening directive."
  (or (= pos 0)
      (let ((before (aref line (1- pos))))
        (or (memq before '(?\s ?\t ?\n))
            (assq before directives)))))

(defun jabber-styling--parse-spans (line)
  "Parse XEP-0393 span directives in LINE.
Return a list of (START END FACE) triples for styled regions.
Matching is lazy (left-to-right, first valid close wins).
Preformatted spans suppress inner directives; other spans may
contain child spans."
  (let ((spans nil)
        (pos 0)
        (len (length line))
        (directives '((?* . jabber-styling-bold)
                      (?_ . jabber-styling-italic)
                      (?~ . jabber-styling-strike)
                      (?` . jabber-styling-pre))))
    (while (< pos len)
      (let* ((ch (aref line pos))
             (face (cdr (assq ch directives))))
        (if (or (not face)
                (not (jabber-styling--valid-opening-p line pos directives)))
            (setq pos (1+ pos))
          ;; Opening must not be followed by whitespace
          (if (or (>= (1+ pos) len)
                  (memq (aref line (1+ pos)) '(?\s ?\t ?\n)))
              (setq pos (1+ pos))
            (let ((close (jabber-styling--find-closing line ch pos len)))
              (if (not close)
                  (setq pos (1+ pos))
                (push (list pos (1+ close) face) spans)
                (setq pos (1+ close))))))))
    (nreverse spans)))

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
         ;; Inside a preformatted block: only exact ``` closes it
         (in-pre
          (when (eq kind 'pre-close)
            (push (list 'pre pre-start line-end) blocks)
            (setq in-pre nil)))
         ;; Opening a preformatted block
         ((memq kind '(pre-open pre-close))
          (setq in-pre t
                pre-start offset))
         ;; Block quote line
         ((eq kind 'quote)
          (push (list 'quote offset line-end) blocks))
         ;; Plain line
         (t
          (push (list 'plain offset line-end) blocks)))
        (setq offset line-end)))
    ;; Unclosed pre block extends to end
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
           ;; Apply quote face to the whole line, then parse spans
           ;; in the content after stripping the > prefix
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

;;; Body printer integration

(declare-function jabber-chat-normal-body "jabber-chat" (msg who mode))

(defun jabber-styling--body-printer (msg who mode)
  "Insert body from MSG with XEP-0393 styling applied.
WHO indicates the sender, MODE is :insert or :printp.
Delegates to `jabber-chat-normal-body' for insertion, then
applies styling to the inserted region."
  (let ((body (plist-get msg :body)))
    (when body
      (if (or (not (eq mode :insert))
              (not jabber-styling-enable)
              (plist-get msg :unstyled)
              (string-prefix-p "/me " body))
          ;; No styling needed: delegate to original printer
          (jabber-chat-normal-body msg who mode)
        ;; Insert body via original, then apply styling
        (let ((start (point)))
          (jabber-chat-normal-body msg who mode)
          (jabber-styling--apply-region start (point)))))))

(with-eval-after-load "jabber-chat"
  (defvar jabber-body-printers)
  ;; Replace jabber-chat-normal-body with our styling-aware version
  (setq jabber-body-printers
        (mapcar (lambda (fn)
                  (if (eq fn 'jabber-chat-normal-body)
                      'jabber-styling--body-printer
                    fn))
                jabber-body-printers)))

;;; Disco

(jabber-disco-advertise-feature jabber-styling-xmlns)

(provide 'jabber-styling)
;;; jabber-styling.el ends here
