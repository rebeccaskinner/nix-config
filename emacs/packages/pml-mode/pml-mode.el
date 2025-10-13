;;; pml-mode.el --- Small layer over markdown-mode for PML -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'markdown-mode)
(require 'subr-x)
(require 'cl-lib)   ;; you use cl-find-if later

;; --- histories ---------------------------------------------------------------

(defvar-local pml-tag-contents-history nil)
(defvar-local pml-tag-name-history nil)
(defvar-local pml-code-block-history nil)
(defvar-local pml-inline-code-history nil)

;; --- helpers ----------------------------------------------------------------

(defun pml--insert-tag-with-value (tag val)
  (insert (format "<%s>%s</%s>" tag val tag)))

(defun pml-make-tag ()
  "Prompt for a tag name and contents, then insert <tag>contents</tag>."
  (interactive)
  (let ((tag (read-string "tag: " nil 'pml-tag-name-history)))
    (add-to-history 'pml-tag-name-history tag)
    (let ((contents (read-string "contents: " nil 'pml-tag-contents-history)))
      (add-to-history 'pml-tag-contents-history contents)
      (pml--insert-tag-with-value tag contents))))

(defun pml-insert-lambda ()
  "Insert a literal lambda."
  (interactive)
  (insert "λ"))

(defun pml-add-backtick-code ()
  "Insert inline code using backticks. If region is active, wrap it."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning)) (end (region-end)))
        (save-excursion
          (goto-char end) (insert "`")
          (goto-char beg) (insert "`")))
    (let ((code (read-string "code: " nil 'pml-inline-code-history)))
      (insert (format "`%s`" code)))))

(defun pml--insert-code-block-lines ()
  (insert "~~~") (newline-and-indent)
  (newline-and-indent)
  (insert "~~~") (forward-line -1))

(defun pml-insert-code-block (&optional lang)
  "Insert a PML code block.  With region active, wrap region as contents.  Prompt for LANG (stored as {:language=\"LANG\"})."
  (interactive)
  (let* ((default (car pml-code-block-history))
         (lang (or lang (read-string (format "language%s: "
                                             (if default (format " (default %s)" default) ""))
                                     nil 'pml-code-block-history default))))
    (add-to-history 'pml-code-block-history lang)
    (if (use-region-p)
        ;; Wrap region:
        (let ((beg (region-beginning)) (end (region-end)))
          ;; insert header
          (goto-char beg)
          (insert (format "{:language=\"%s\"}\n" lang))
          ;; opening fence goes AFTER the header we just inserted
          (insert "~~~\n")
          ;; now place closing fence at end of region
          (save-excursion
            (goto-char end)
            (let ((end-marker (copy-marker (point) t)))
              (goto-char end-marker)
              (insert "\n~~~\n"))))
      (insert (format "{:language=\"%s\"}\n" lang))
      (pml--insert-code-block-lines)
      (end-of-line) (newline-and-indent))))

;; --- minor mode -------------------------------------------------------------

(defvar pml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l") #'pml-insert-lambda)
    (define-key map (kbd "C-c t") #'pml-make-tag)
    (define-key map (kbd "C-c b") #'pml-insert-code-block)
    (define-key map (kbd "C-c m") #'pml-add-backtick-code)
    (define-key map (kbd "C-c e") #'pml-embed-file)
    (define-key map (kbd "<tab>") #'pml-contextual-tab)
    map)
  "Keymap for `pml-mode'.")

;; Evil states (so normal/visual/motion use it *only when pml-mode is active*)
(with-eval-after-load 'evil
  ;; Ex command: :embed-file
  (when (fboundp 'evil-ex-define-cmd)
    (evil-ex-define-cmd "embed-file" #'pml-embed-file))

  ;; Local TAB/C-i override inside pml-mode
  (evil-define-key* '(normal visual motion) pml-mode-map
    (kbd "TAB")   #'pml-contextual-tab
    (kbd "<tab>") #'pml-contextual-tab
    (kbd "C-i")   #'pml-contextual-tab))

;;;###autoload
(define-minor-mode pml-mode
  "Publisher Markdown layer on top of `markdown-mode`."
  :lighter " PML"
  :keymap pml-mode-map
  (if pml-mode
      (progn
        (visual-line-mode 1)
        (flyspell-mode 1)
        ;; Light highlighting to spot PML bits quickly:
        (font-lock-add-keywords
         nil
         '(("\\[aside\\b[^]]*\\]" . 'markdown-markup-face)
           ("\\[/aside\\]"       . 'markdown-markup-face)
           ("<ed\\b[^>]*>"       . 'markdown-markup-face))
         'append)
        (font-lock-flush))
    (font-lock-remove-keywords
     nil
     '(("\\[aside\\b[^]]*\\]" . 'markdown-markup-face)
       ("\\[/aside\\]"        . 'markdown-markup-face)
       ("<ed\\b[^>]*>"        . 'markdown-markup-face)))
    (font-lock-flush)))

;; --- auto-enable for *.pml --------------------------------------------------

(defun pml--maybe-enable ()
  (when (and buffer-file-name
             (string-match-p "\\.pml\\'" buffer-file-name))
    (pml-mode 1)))

;; Make *.pml open in markdown-mode, then layer PML.
(add-to-list 'auto-mode-alist '("\\.pml\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'pml--maybe-enable)

(defun pml--book-root ()
  "Return the directory where the current .pml file lives."
  (file-name-directory (or buffer-file-name default-directory)))


(defun pml--collect-parts-in-file (path)
  "Return a list of part names found in PATH.
Matches ANY occurrence of START:<name> (optionally with <name> in angle brackets)."
  (let (parts)
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (let ((case-fold-search nil)) ;; be explicit; markers are uppercase
          (while (re-search-forward
                  "START:\\s-*<?\\([A-Za-z0-9._-]+\\)>?"
                  nil t)
            (push (match-string 1) parts)))))
    (nreverse parts)))

(defun pml--read-file ()
  "Prompt for a file path inside Book/."
  (read-file-name "Embed file: " (pml--book-root) nil t))

(defun pml--read-part (parts)
  "Read a part from PARTS or return nil for whole file."
  (cond
   ((null parts) nil)
   (t
    (let* ((choices (append '("<<whole file>>") parts))
           (sel (completing-read "Part: " choices nil t nil nil "<<whole file>>")))
      (unless (string= sel "<<whole file>>") sel)))))

(defun pml--yes-no (prompt &optional default-yes)
  "Simple yes/no PROMPT returning t/nil.  DEFAULT-YES means RET = yes."
  (let ((ans (read-from-minibuffer
              (format "%s %s " prompt (if default-yes "[Y/n]" "[y/N]")))))
    (cond
     ((string-empty-p ans) default-yes)
     ((string-match-p "\\`[Yy]" ans) t)
     (t nil))))

;;;###autoload
(defun pml-embed-file ()
  "Insert a <embed …/> tag, optionally selecting a START:<partname>."
  (interactive)
  (let* ((abs (expand-file-name (pml--read-file)))
         (rel (pml--relative-to-book abs))   ;; <-- new
         (parts (pml--collect-parts-in-file abs))
         (part (pml--read-part parts))
         (showname-no (pml--yes-no "Add showname=\"no\"?" nil))
         (attrs (string-join
                 (delq nil
                       (list (format "file=\"%s\"" rel)
                             (when part (format "part=\"%s\"" part))
                             (when showname-no "showname=\"no\"")))
                 " ")))
    (insert (format "<embed %s/>" attrs))))

;; Keybinding in your minor mode map (adjust if you named it differently):

;; --- embed preview toggle ----------------------------------------------------
(defconst pml-embed-line-regexp "^\\s-*<embed\\b")

(defun pml--embed-at-point ()
  "If point is on an <embed …/> line, return plist with :bol :eol :file :part.
Returns nil if not on a valid embed line."
  (save-excursion
    (beginning-of-line)
    ;; First, check if line starts with <embed
    (when (looking-at pml-embed-line-regexp)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (line (buffer-substring-no-properties bol eol))
             (attrs (pml--extract-attrs line))
             (file  (alist-get "file" attrs nil nil #'string=))
             (part  (alist-get "part" attrs nil nil #'string=)))
        (when file
          (list :bol bol :eol eol :file file :part part))))))

(defun pml--resolve-path (rel)
  "Resolve REL relative to Book/."
  (expand-file-name rel (pml--book-root)))

(defun pml--relative-to-book (abs)
  "Return ABS as a path relative to the Book/ directory."
  (file-relative-name abs (pml--book-root)))

(defun pml--extract-attrs (line)
  "Return alist of key . value from an <embed …> LINE."
  (let (alist)
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (while (re-search-forward "\\([A-Za-z0-9_-]+\\)=\"\\([^\"]*\\)\"" nil t)
        (push (cons (match-string 1) (match-string 2)) alist)))
    (nreverse alist)))

(defun pml--extract-part (abs part)
  "Return text for PART in ABS between START:PART and END:PART.
If PART is nil, return the whole file."
  (unless (file-readable-p abs)
    (user-error "File not readable: %s" abs))
  (with-temp-buffer
    (insert-file-contents abs)
    (if (not part)
        (buffer-string)
      (let* ((case-fold-search nil)
             (name    (regexp-quote part))
             (start-re (concat "START:\\s-*<?" name ">?"))
             (end-re   (concat   "END:\\s-*<?" name ">?")))
        (goto-char (point-min))
        ;; Find first START:part anywhere in the file
        (unless (re-search-forward start-re nil t)
          (user-error "Part %s not found in %s" part abs))
        ;; Content begins AFTER the start line
        (let ((beg (line-end-position)))
          ;; Find the matching END:part AFTER beg
          (unless (re-search-forward end-re nil t)
            (user-error "END:%s not found in %s" part abs))
          ;; Content ends BEFORE the end line
          (buffer-substring-no-properties beg (line-beginning-position)))))))

(defun pml--code-block-string (text abs &optional part)
  "Pretty preview string for TEXT from ABS, preserving syntax highlighting.
Adds a subtle header and background without inserting real fences."
  (let* ((body (pml--fontify-like-file (string-trim-right text) abs))
         (file (file-name-nondirectory abs))
         (hdr  (format "⟦ %s%s ⟧"
                       file
                       (if (and part (not (string-empty-p part)))
                           (format " : %s" part) ""))))
    ;; Add a header line and a blank line before the code for breathing room.
    (setq hdr (concat "\n" (propertize hdr 'face 'pml-preview-header-face) "\n"))
    ;; Give the whole body a background while keeping existing syntax faces.
    (add-face-text-property 0 (length body) 'pml-preview-face 'append body)
    ;; Ensure a trailing newline so the block doesn’t run into following text.
    (concat hdr body "\n")))


;; 6) find an existing preview specifically at EOL (your simple rule)
(defun pml--preview-at-eol ()
  (let ((eol (line-end-position)))
    (cl-find-if (lambda (ov) (overlay-get ov 'pml-embed-preview))
                (overlays-in eol eol))))

(defun pml-toggle-embed-preview ()
  "Toggle inline preview for <embed …/> on the current line.  Return t if handled."
  (interactive)
  (let ((info (pml--embed-at-point)))
    (when info
      (let* ((eol (plist-get info :eol))
             (existing (pml--preview-at-eol)))
        (message "[pml] existing: %s\n" existing)
        (if existing
            (delete-overlay existing)
          (let* ((abs  (pml--resolve-path (plist-get info :file)))
                 (part (let ((p (plist-get info :part)))
                         (and (stringp p) (not (string-empty-p p)) p)))
                 (txt  (pml--extract-part abs part))
                 (ov   (make-overlay eol eol nil t t))  ;; zero-length, sticky
                 (disp (pml--code-block-string txt abs part)))
            (overlay-put ov 'pml-embed-preview t)
            (overlay-put ov 'priority 1001)
            (overlay-put ov 'after-string disp))))
      t)))

(defface pml-preview-face
  '((t :inherit default :background "#222" :extend t))
  "Background face for PML inline previews.")

(defface pml-preview-header-face
  '((t :inherit shadow :weight bold))
  "Face for the small header line above previews.")

(defun pml--fontify-like-file (text abs-path)
  "Return TEXT with font-lock faces as if it were in ABS-PATH's major mode."
  (with-temp-buffer
    (insert text)
    ;; Pretend to be visiting ABS-PATH just long enough to pick the mode.
    (let ((buffer-file-name abs-path))
      (delay-mode-hooks (set-auto-mode t)))
    ;; Clear so Emacs doesn’t think this buffer is visiting a file.
    (setq buffer-file-name nil)
    (when (boundp 'font-lock-mode)
      (font-lock-mode 1))
    (font-lock-ensure (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun pml-clear-all-previews ()
  "Remove all overlay previews."
  (interactive)
  (remove-overlays nil nil 'pml-embed-preview t))

(defun pml-contextual-tab ()
  "If on an <embed …/> line, toggle preview; otherwise do the usual thing.
Always prints whether it detected an embed line."
  (interactive)
  (let ((info (pml--embed-at-point)))
    (if info
        (progn
          (pml-toggle-embed-preview)
          (message nil)) ;; clear the echo area after toggling
      ;; Fallbacks exactly as before:
      (cond
       ((and (bound-and-true-p evil-local-mode)
             (or (evil-normal-state-p) (evil-motion-state-p))
             (fboundp 'evil-jump-forward))
        (call-interactively #'evil-jump-forward))
       ((derived-mode-p 'markdown-mode)
        (call-interactively #'markdown-cycle))
       (t
        (call-interactively #'indent-for-tab-command))))))

(provide 'pml-mode)
;;; pml-mode.el ends here
