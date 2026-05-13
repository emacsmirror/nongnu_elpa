;;; elfeed-tree.el --- Show feeds as a tree structure -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Daniel Mendler <mail@daniel-mendler.de>

;;; Commentary:

;; The elfeed tree buffer gives an overview over all feeds and tags.
;; The feeds are visualized as a tree using the feed auto tags.  The
;; first auto tag is the root node, the second tag comes below, and so
;; on.  Feeds with the same auto tags are grouped together.
;;
;; For example the configuration
;;
;;     (setq elfeed-feeds
;;       '(("https://yhetil.org/emacs-devel/new.atom" emacs lists devel)
;;         ("https://yhetil.org/emacs-bugs/new.atom" emacs lists bugs)
;;         ("https://old.reddit.com/r/emacs.rss" emacs reddit)))
;;
;; will lead to a tree of the following form.
;;
;;     emacs
;;       ├─● /r/reddit
;;       lists
;;         ├─● emacs-devel
;;         ╰─● emacs-bugs
;;     [all feeds]
;;       ├─● …
;;       ╰─● emacs-devel
;;     [all tags]
;;       ├─● …
;;       ╰─● emacs
;;            ├─● …
;;            ╰─● emacs-devel
;;
;; Unfold the tree nodes with TAB or S-TAB, jump to an entry via RET
;; or by clicking.  This feature has been inspired by the
;; elfeed-summary package.

;;; Code:

(eval-when-compile (require 'subr-x))

(require 'elfeed)
(require 'elfeed-search)
(require 'outline)

(defface elfeed-tree-highlight-unread-face
  '((t :inherit warning))
  "Face used in tree mode to highlight unread entries."
  :group 'elfeed)

(defcustom elfeed-tree-filter "@6months"
  "Query string added to filter."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-tree-nodes ["├─●" "╰─●" "│  " "   "]
  "Strings used to visualize nodes of the tree."
  :group 'elfeed
  :type '(vector string string string string))

(defvar elfeed-tree--update-timer nil
  "Timer to debounce search buffer updates.")

(defvar elfeed-tree--last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar-keymap elfeed-tree-mode-map
  :doc "Keymap for `elfeed-tree-mode'."
  :parent special-mode-map
  "RET" #'elfeed-tree-search
  "<mouse-1>" #'elfeed-tree-click
  "<header-line> <mouse-1>" #'elfeed-search-header-click
  "n" #'next-line
  "p" #'previous-line
  "T" #'elfeed-tree-set-title
  "G" #'elfeed-update
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer)

(easy-menu-define elfeed-tree-mode-menu elfeed-tree-mode-map
  "Menu for `elfeed-tree-mode'."
  '("Elfeed Tree"
    ["Search for feed or tag" elfeed-tree-search]
    ["Set feed title" elfeed-tree-set-feed-title]
    "--"
    ["Fetch all" elfeed-search-fetch]
    ["Fetch feed" elfeed-update-feed]
    "--"
    ("Maintenance"
     ["Apply auto tags" elfeed-apply-autotags-now]
     ["Apply hooks" elfeed-apply-hooks-now]
     ["Compact database" elfeed-db-compact]
     ["Unjam queue" elfeed-unjam])
    "--"
    ["Revert buffer" revert-buffer]
    ["Quit window" quit-window]
    "--"
    ["Customize" (customize-group 'elfeed)]))

(defun elfeed-tree-click (event)
  "Handle click EVENT in `elfeed-tree' buffer."
  (declare (completion ignore))
  (interactive "@e")
  (when-let* ((pos (event-end event))
              (pos (posn-point pos))
              (obj (or (get-text-property pos 'elfeed-filter)
                       (get-text-property pos 'elfeed-tag))))
    (elfeed-tree-search obj)))

(defun elfeed-tree-search (filter-or-tag)
  "Go to search buffer limited to FILTER-OR-TAG at point.
FILTER-OR-TAG can either be a filter string, a single tag symbol or a
list of tag symbols."
  (interactive (list (or (get-text-property (pos-bol) 'elfeed-filter)
                         (get-text-property (pos-bol) 'elfeed-tag)
                         (user-error "No feed or tag at point"))))
  (push-mark)
  (elfeed-search)
  (elfeed-search-set-filter
   (concat
    elfeed-tree-filter
    (and (not (equal elfeed-tree-filter "")) " ")
    (if (stringp filter-or-tag)
        filter-or-tag
      (elfeed-search--tag-filter filter-or-tag)))))

(defun elfeed-tree-set-title (feed title)
  "Set TITLE of FEED at point."
  (interactive
   (let ((feed (or (get-text-property (pos-bol) 'elfeed-feed)
                   (user-error "No feed at point"))))
     (list feed (read-from-minibuffer "Feed title: "
                                      (elfeed-meta--title feed)))))
  (setf (elfeed-meta feed :title) title)
  (elfeed-tree-update :force))

(defun elfeed-tree--header ()
  "Computes the string to be used in the header line."
  (let ((elfeed-search-filter-active t)
        (elfeed-search-filter-overflowing t))
    ;; Reuse the header from the search buffer here
    ;; without the unread count and the search filter.
    (elfeed-search--header)))

(define-derived-mode elfeed-tree-mode special-mode "elfeed-tree"
  "Major mode for listing elfeed feeds as a tree."
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq-local truncate-lines t
              revert-buffer-function #'elfeed-tree--update-force
              default-directory (elfeed-default-directory)
              outline-regexp "\\*+"
              outline-minor-mode-cycle t
              ;; Provide format string via symbol value slot so that it will
              ;; not be %-construct interpolated. The symbol is uninterned
              ;; so that it's not *really* a global variable.
              header-line-format
              (let ((symbol (make-symbol "dummy")))
                (put symbol 'risky-local-variable t)
                `(:eval
                  (prog1 ',symbol
                    (set ',symbol (elfeed-tree--header))))))
  (outline-minor-mode)
  (buffer-disable-undo)
  (hl-line-mode)
  (add-hook 'elfeed-untag-hooks #'elfeed-tree--tag)
  (add-hook 'elfeed-tag-hooks #'elfeed-tree--tag)
  (add-hook 'elfeed-update-hooks #'elfeed-tree--update-debounce)
  (add-hook 'elfeed-update-init-hooks #'elfeed-tree--update-force)
  (add-hook 'kill-buffer-hook #'elfeed-db-save t 'local)
  (add-hook 'quit-window-hook 'elfeed-db-save nil 'local)
  (add-hook 'elfeed-db-unload-hook #'elfeed-tree--unload)
  (elfeed-tree-update :force)
  (outline-minor-mode))

;;;###autoload
(defun elfeed-tree ()
  "Enter `elfeed-tree' buffer."
  (interactive)
  (switch-to-buffer (elfeed-tree--buffer))
  (unless (eq major-mode 'elfeed-tree-mode)
    (elfeed-tree-mode)))

(defun elfeed-tree--buffer ()
  "Create and return tree buffer."
  (get-buffer-create "*elfeed-tree*"))

(defun elfeed-tree--unload ()
  "Hook function for `elfeed-db-unload-hook'."
  (with-current-buffer (elfeed-tree--buffer)
    ;; don't try to save the database in this case
    (remove-hook 'kill-buffer-hook #'elfeed-db-save t)
    (kill-buffer)))

(defun elfeed-tree--tag (_entries tags)
  "Refresh if unread TAGS have changed."
  (when (memq 'unread tags)
    (setq elfeed-tree--last-update 0)
    (elfeed-tree-update)))

(defun elfeed-tree--update-force (&rest _)
  "Call `elfeed-tree-update' with argument :force.
The function is used as hook.  Instead of this function, you usually
want to use `elfeed-tree-update'."
  (elfeed-tree-update :force))

(defun elfeed-tree--update-debounce (&rest _)
  "Call `elfeed-tree-update' with debouncing.
The function is used as hook.  Instead of this function, you usually
want to use `elfeed-tree-update'."
  (elfeed-tree-update))

(defun elfeed-tree-update (&optional force)
  "Update the `elfeed-tree' buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed.
Otherwise debounce by `elfeed-search-update-delay' and only redraw when
there are changes.  When called interactively FORCE is t, and the
command behaves just like `revert-buffer'."
  (when elfeed-tree--update-timer
    (cancel-timer elfeed-tree--update-timer)
    (setq elfeed-tree--update-timer nil))
  (when-let* ((buffer (get-buffer "*elfeed-tree*")))
    (if force
        (elfeed-tree--update-immediately buffer :force)
      (setf elfeed-tree--update-timer
            (run-at-time elfeed-search-update-delay nil
                         #'elfeed-tree--update-immediately buffer)))))

(defun elfeed-tree--sort (nodes)
  "Sort tree NODES by unread count and name."
  (sort nodes (lambda (x y)
                (if (= (cadr x) (cadr y))
                    (string< (car x) (car y))
                  (> (cadr x) (cadr y))))))

(defun elfeed-tree--collect ()
  "Collect list of feeds and tags from the database.
Returns a pair of two lists of the format.  The feed list entries have
the format (title unread read feed tags).  The tag list entries have the
format (tag unread read)."
  (let ((feeds-ht (make-hash-table :test #'equal))
        (tags-ht (make-hash-table :test #'eq)))
    (elfeed-db-visit (entry feed)
      (let* ((tags (elfeed-entry-tags entry))
             (unread (memq 'unread tags))
             (feed-id (elfeed-feed-id feed)))
        ;; Collect tags in tags hash table.
        (dolist (tag tags)
          (unless (hash-table-contains-p tag tags-ht)
            (puthash tag (list tag 0 0) tags-ht))
          (if unread
              (incf (cadr (gethash tag tags-ht)))
            (incf (caddr (gethash tag tags-ht)))))
        ;; Collect feeds in feeds hash table.
        (unless (hash-table-contains-p feed-id feeds-ht)
          (puthash feed-id (list (elfeed-meta--title feed) 0 0 feed
                                 (elfeed-feed-autotags feed))
                   feeds-ht))
        (if unread
            (incf (cadr (gethash feed-id feeds-ht)))
          (incf (caddr (gethash feed-id feeds-ht))))))
    (cons (hash-table-values feeds-ht) (hash-table-values tags-ht))))

(defun elfeed-tree--build-nested (nodes)
  "Build a nested tree from a flat list of NODES.
For each node the list of tags is taken and turned into parent nodes."
  (let (children leaves)
    (cl-loop
     for (title unread read feed tags) in nodes
     for item = (list title unread read feed (cdr tags)) do
     (if (car tags)
         (push item (alist-get (car tags) children))
       (push item leaves)))
    (cl-loop for x in children do
             (cl-callf elfeed-tree--build-nested (cdr x)))
    (list children leaves)))

(defun elfeed-tree--build-tags (feeds tags stats)
  "Build an all tags tree from the list of all FEEDS and TAGS.
STATS is the unread/read/count statistics."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (feed feeds)
      (dolist (tag (nth 4 feed))
        (push feed (gethash tag ht))))
    (let ((children
           (cl-loop
            for (tag unread read) in tags
            for feeds = (gethash tag ht)
            collect (list tag unread read (length feeds) nil feeds))))
      `(("[all tags]" ,@stats ,children nil)))))

(defun elfeed-tree--depth (nodes)
  "Compute tree depth given a list of NODES."
  (if nodes
      (cl-loop for (_tag _unread _read _count children _leaves) in nodes
               maximize (if children (1+ (elfeed-tree--depth children)) 0))
    0))

(defun elfeed-tree--stats (nodes)
  "Compute sum of unread and read counts for parent nodes.
NODES is a list of tree nodes."
  (cl-loop
   for (tag children leaves) in nodes
   for rec = (elfeed-tree--stats children)
   collect
   (list
    tag
    ;; Sum unread counts from children and leaves
    (+ (if rec (cl-loop for (_title ur _read . _) in rec sum ur) 0)
       (if leaves (cl-loop for (_title ur _read . _) in leaves sum ur) 0))
    ;; Sum read counts from children and leaves
    (+ (if rec (cl-loop for (_title _ur read . _) in rec sum read) 0)
       (if leaves (cl-loop for (_title _ur read . _) in leaves sum read) 0))
    ;; Sum feed number from children and leaves
    (+ (if rec (cl-loop for (_title _ur _read count . _) in rec sum count) 0)
       (length leaves))
    rec leaves)))

(defun elfeed-tree--flatten (nodes)
  "Flatten tree NODES."
  (cl-loop
   for (tag children leaves) in nodes collect
   (progn
     ;; Merge nodes if a node has only a single subnode.
     (while (and (not leaves) (length= children 1))
       (setq tag (format "%s %s" tag (caar children))
             leaves (caddar children)
             children (cadar children)))
     ;; Drop parent nodes with only a single feed as leaf.
     ;; Keep these nodes with this code:
     ;;  (list tag (elfeed-tree--flatten children) leaves)
     (cl-loop for entry in (elfeed-tree--flatten children)
              for (_tg ts us) = entry
              if (and (not ts) (length= us 1)) collect (car us) into new-leaves
              else collect entry into new-children
              finally return
              (list tag new-children (nconc leaves new-leaves))))))

(defun elfeed-tree--count-unread (unread read)
  "Format unread/total count for a feed line given UNREAD and READ."
  (format "%4s/%-6s"
          (if (> unread 0)
              (format
               (propertize "%s" 'face 'elfeed-tree-highlight-unread-face)
               unread)
            unread)
          (+ unread read)))

(defun elfeed-tree--node (idx)
  "Return string for the tree visualization given IDX."
  (propertize (if (< idx 2) " " "*") 'display
              (substring (aref elfeed-tree-nodes idx))))

(defun elfeed-tree--title (indent title unread read count tags)
  "Insert TITLE into buffer.
INDENT is the indentation prefix, UNREAD and READ the respective counts,
COUNT the number of feeds and TAGS the list of tags."
  (let ((title (format "%s (%s/%s:%s)"
                       title
                       (if (> unread 0)
                           (format
                            (propertize
                             "%s" 'face 'elfeed-tree-highlight-unread-face)
                            unread)
                         unread)
                       (+ unread read)
                       count))
        (level (length indent)))
    (add-face-text-property 0 (length title)
                            (aref outline-font-lock-faces
                                  (1- (min level (length outline-font-lock-faces))))
                            'append title)
    (insert (propertize
             (concat indent title)
             'elfeed-tag (if (and (> unread 0) (not (memq 'unread tags)))
                             `(,@tags unread)
                           tags)
             'mouse-face 'highlight)
            "\n")))

(defun elfeed-tree--print (indent tags title-fmt depth nodes)
  "Print tree NODES.
INDENT is the current indentation prefix string.
TAGS the list of outer tags which are added to the filter.
TITLE-FMT the format string for the feed title.
DEPTH the tree depth."
  (setq indent (or indent (propertize "*" 'invisible t)))
  (cl-loop
   for level = (length indent)
   for (tag unread read count children leaves) in (elfeed-tree--sort nodes)
   for node-idx downfrom (length nodes) do
   (let ((subtags (append tags
                          (if (stringp tag)
                              (unless (string-prefix-p "[" tag)
                                (mapcar #'intern (split-string tag)))
                            (list tag))))
         (subindent (concat indent
                            (elfeed-tree--node
                             (if (or (= level 1) (= node-idx 1)) 3 2)))))
     (elfeed-tree--title indent tag unread read count subtags)
     (cl-loop
      for (title unread read feed _tags) in (elfeed-tree--sort leaves)
      for leaf-idx downfrom (length leaves) do
      (insert
       (elfeed-add-properties
        (concat subindent
                (elfeed-tree--node
                 (if (and (not children) (= leaf-idx 1)) 1 0))
                (mapconcat (lambda (_)
                             (propertize
                              " " 'display
                              (substring (aref elfeed-tree-nodes 3))))
                           (make-list (- depth level -1) 0))
                (elfeed-tree--count-unread unread read)
                (format title-fmt title)
                " " (elfeed-feed-id feed))
        'elfeed-feed feed
        'elfeed-filter (concat (elfeed-search--feed-filter feed)
                               (and (> unread 0) " +unread"))
        'mouse-face 'highlight)
       "\n"))
     (elfeed-tree--print subindent subtags title-fmt depth children))))

(defun elfeed-tree--update-immediately (buffer &optional force)
  "Immediately update the `elfeed-tree' BUFFER.
If FORCE is nil, only refresh the buffer when the database changed.  Do
not use this function directly.  Instead use `elfeed-tree-update'."
  (when (and (buffer-live-p buffer)
             (or force (< elfeed-tree--last-update (elfeed-db-last-update))))
    (with-current-buffer buffer
      (save-excursion
        (let* ((inhibit-read-only t)
               (feeds+tags (elfeed-tree--collect))
               (feeds (car feeds+tags))
               (tags (cdr feeds+tags))
               (nodes (elfeed-tree--build-nested feeds))
               (tree (elfeed-tree--stats (elfeed-tree--flatten (car nodes))))
               (tree-depth (max 2 (elfeed-tree--depth tree)))
               (untagged-feeds-tree
                (elfeed-tree--stats
                 `(("[untagged feeds]" nil . ,(cdr nodes)))))
               (all-feeds-tree
                (elfeed-tree--stats
                 `(("[all feeds]" nil ,feeds))))
               (title-width (cl-loop for (title . _) in feeds
                                     maximize (length title)))
               (title-fmt (propertize (format "%%-%ds" title-width)
                                      'face 'elfeed-search-feed-face)))
          (erase-buffer)
          (goto-char (point-min))
          (elfeed-tree--print nil nil title-fmt tree-depth tree)
          (elfeed-tree--print nil nil title-fmt tree-depth untagged-feeds-tree)
          (elfeed-tree--print nil nil title-fmt tree-depth all-feeds-tree)
          (elfeed-tree--print nil nil title-fmt tree-depth
                              (elfeed-tree--build-tags
                               feeds tags
                               (take 3 (cdar all-feeds-tree))))
          (outline-hide-sublevels 1)
          (setq elfeed-tree--last-update (float-time))))))
  ;; Always force a header line update
  (force-mode-line-update))

(provide 'elfeed-tree)
;;; elfeed-tree.el ends here
