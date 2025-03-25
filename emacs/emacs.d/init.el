;;; init --- Emacs configuration
;;; provide (init)
;;; Commentary:

;;; Code:
;; Disable the splash screen
(setq inhibit-splash-screen t)

(defun configure-look-and-feel ()
  "Run some stuff after init, like setting a theme and disabling scrollbars."
  ;; Setup theme
  (load-theme 'darkplum t)
  ;; (load-theme 'dracula t)

  ;; disable the menu bar
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  )

  (require 'evil)
  (evil-mode 1)

(defun deamon-look-and-feel (frame)
  "Wrapper to run look-and-feel per FRAME with emacsclient."
  (select-frame frame)
  (configure-look-and-feel)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions #'deamon-look-and-feel)
  (configure-look-and-feel)
  )

;; Set the face for the current frame
(set-face-attribute 'default nil :family "FiraCode" :foundry "ADBO" :height 130)

;; Set the default browser to firefox
(setq browse-url-browser-function 'browse-url-firefox)

;; global custom commands
(require 'calendar)

(defun timestamp ()
  "Insert a timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

;; Show the current time in the modeline
(display-time-mode 1)


(use-package direnv
  :config
  (direnv-mode))

(defun setup-global-keybindings()
  "Setup global keybindings."
  (global-set-key (kbd "M-P") 'ace-window)
  (global-set-key (kbd "<M-up>") 'ace-window)
  (global-set-key (kbd "C-'") 'goto-last-change)
  (global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-\"") "“")
  (global-set-key (kbd "M-\"") "”")
  )

(setup-global-keybindings)

(defun configure-temp-files()
  "Set the auto-save and backup files to /tmp/."
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  )

(configure-temp-files)

(setq-default indent-tabs-mode nil)

(defun configure-ivy-mode ()
  "Configure ivy-mode and set up a few keybindings."
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
;  (global-set-key (kbd "C-s") 'swiper-isearch)
  )

(configure-ivy-mode)

;; Turn on visual line-wrapping mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'tex-mode-hook 'turn-on-visual-line-mode)

(defun enable-orgmode-graphiz-execution()
  "Enable dot rendering and editing with org-bable."
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (haskell . t)
     (shell . t)
     )) ; this line activates dot
  )

(defun enable-orgmode-inline-preview()
  "Turn on inline preview in orgmode."
  (setq org-start-with-inline-images t)
  )

(defun enable-orgmode-ruby-execution()
  "Enable ruby rendering and execution with org-bable."
  (require 'ob-ruby)
  )

(defun disable-org-auto-indent()
  "Disable automatic indentation of sections in org mode."
  (setq org-adapt-indentation nil)
  )

(defun configure-org-agenda()
  "Configure org mode agenda with agenda files and keybindings."
  (setq org-agenda-files (list "~/agenda.org"))
  (global-set-key (kbd "C-c a") 'org-agenda)
  )

(configure-org-agenda)
(add-hook 'org-mode-hook 'disable-org-auto-indent)
(add-hook 'org-mode-hook 'enable-org-reveal)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'enable-orgmode-graphiz-execution)
(add-hook 'org-mode-hook 'enable-orgmode-inline-preview)
(add-hook 'org-mode-hook 'enable-orgmode-ruby-execution)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Rainbow Delimiters
(require 'rainbow-delimiters)

;;; Setup Fill-Mode
(require 'fill-column-indicator)

;; Visual fci config
(setq fci-rule-width 1)
(setq fci-rule-color "darkgrey")

;; Turn on fci mode by default
(add-hook 'after-init-hook 'fci-mode)

(defun line-number-config()
  "Configure line numbers."
  (defun absolute-line-numbers()
    (interactive)
    (setq display-line-numbers-type t)
    (display-line-numbers-mode)
    )

  (defun relative-line-numbers()
    (interactive)
    (setq display-line-numbers-type 'relative)
    (display-line-numbers-mode)
    )

  (defun visual-line-numbers()
    (interactive)
    (setq display-line-numbers-type 'visual)
    (display-line-numbers-mode)
    )

  (defun turn-off-line-numbers()
    (interactive)
    (setq display-line-numbers nil)
    )

  (defun turn-on-line-numbers()
    (interactive)
    (display-line-numbers-mode)
    )

  (defun toggle-line-numbers()
    (interactive)
    (if (eq display-line-numbers nil)
        (turn-on-line-numbers)
      (turn-off-line-numbers)
      )
    )
  (global-set-key (kbd "C-c n") 'toggle-line-numbers)
  )

(line-number-config)

(defun soft-wrap-config (&optional width)
  "Configure soft-wrap to WIDTH columns of text, and set a visual fill column at the boundry."
  (unless width (setq width 80))
  (set-fill-column width)
  (fci-mode 1)
  (auto-fill-mode -1)
  (turn-on-visual-line-mode)
  )

(defun enable-expand-region ()
  "Configures the 'expand-region' command for development modes."
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(enable-expand-region)

;; mode specific configs
(defun default-programming-config ()
  "Configure some sane defaults shared across various programming-related major modes."
  (set-fill-column 80)
  (auto-fill-mode 1)
  (auto-complete-mode 1)
  (rainbow-delimiters-mode 1)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq tab-width 2)
  (local-set-key (kbd "C-)") 'forward-sexp)
  (local-set-key (kbd "C-(") 'backward-sexp)
  (turn-on-line-numbers)
  (set-fill-column 80)
  )

(defun my-dhall-mode-config ()
  "Configure basic settings when editing in dhall-mode."
  (default-programming-config)
  )

(add-hook 'dhall-mode 'my-dhall-mode-config)

;; Extra functions for pml mode
(defun pml-mode-tools()
  "The pml-mode-tools enable some extra functions to make it nicer to edit PML."
  (defvar tag-contents-history '())
  (defvar tag-name-history '())
  (defvar code-block-history '())
  (defvar method-name-history '())
  (interactive)
  (defun insert-tag-with-value(tag val)
    (insert (format "<%s>%s</%s>" tag val tag))
    )
  (defun make-tag()
    (interactive)
    "The make-tag function gets a tag name and value and inserts the tag."
    (let ((tag (read-string "tag: " nil 'tag-name-history )))
      (add-to-history 'tag-name-history tag)
      (let ((contents (read-string "contents: " nil 'tag-contents-history )))
        (add-to-history 'tag-contents-history contents)
        (insert-tag-with-value tag contents)
        )
      )
    )
  (defun insert-code-block-without-contents(lang)
    (insert (format "{:language=\"%s\"}" lang))
    (newline-and-indent)
    (insert "~~~")
    (newline-and-indent)
    (insert "~~~")
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )
  (defun insert-code-block-with-contents(lang contents)
    (insert-code-block-without-contents lang)
    (insert contents)
    (next-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun add-objc-method ()
    "Add an objcmethod tag."
    (interactive)
    (let ((method (read-string "method: " nil 'method-name-history)))
      (insert-tag-with-value "objcmethod" method)
      )
    )

  (defun add-inline-code ()
    "Add an ic tag."
    (interactive)
    (let ((method (read-string "code: " nil 'method-name-history)))
      (insert-tag-with-value "ic" method)
      )
    )

  (defun add-backtick-code ()
    "Add some inline code using backticks"
    (interactive)
    (let ((code (read-string "code: " nil 'method-name-history)))
      (insert (format "`%s`" code))
      )
    )

  (defun add-code-block ()
    "Add a code block without spawning a mini-window."
    (interactive)
    (let ((lang (read-string "language: " nil 'code-block-history)))
      (add-to-history 'code-block-history lang)
      (insert-code-block-without-contents lang)
      )
    )

  (defun insert-lambda ()
    "insert a literal lambda character"
    (interactive)
    (insert "λ")
    )

  (local-set-key (kbd "C-c l") 'insert-lambda)
  (local-set-key (kbd "C-c c") 'add-backtick-code)
  (local-set-key (kbd "C-c t") 'make-tag)
  (local-set-key (kbd "C-c b") 'add-code-block)
  (local-set-key (kbd "C-c m") 'add-backtick-code)
  (local-set-key (kbd "C-c i") 'add-inline-code)
  )

(defun markdown-mode-tools()
  "The markdown-mode-tools enable some extra functions to make it nicer to edit code-focused blog posts in Markdown."

  (defvar tag-contents-history '())
  (defvar tag-name-history '())
  (defvar code-block-history '())
  (defvar inline-code-history '())
  (interactive)

  (defun insert-tag-with-value(tag val)
    (insert (format "<%s>%s</%s>" tag val tag))
    )

  (defun make-tag()
    (interactive)
    "The make-tag function gets a tag name and value and inserts the tag."
    (let ((tag (read-string "tag: " nil 'tag-name-history )))
      (add-to-history 'tag-name-history tag)
      (let ((contents (read-string "contents: " nil 'tag-contents-history )))
        (add-to-history 'tag-contents-history contents)
        (insert-tag-with-value tag contents)
        )
      )
    )

  (defun add-inline-code()
    (interactive)
    "The add-inline-code function gets some code and inserts it."
    (let ((code (read-string "code: " nil 'tag-name-history )))
      (add-to-history 'inline-code-history code)
      (insert (format "`%s`" code))
      )
    )

  (defun insert-code-block-without-contents(lang)
    (insert (format "```%s" lang))
    (newline-and-indent)
    (insert "```")
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun insert-code-block-with-contents(lang contents)
    (insert-code-block-without-contents lang)
    (insert contents)
    (next-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun add-code-block ()
    "Add a code block without spawning a mini-window."
    (interactive)
    (let ((lang (read-string "language: " nil 'code-block-history)))
      (add-to-history 'code-block-history lang)
      (insert-code-block-without-contents lang)
      )
    )

  (local-set-key (kbd "C-c t") 'make-tag)
  (local-set-key (kbd "C-c b") 'add-code-block)
  (local-set-key (kbd "C-c m") 'add-inline-code)
  )

(add-hook 'markdown-mode-hook 'markdown-mode-tools)

;; emacs lisp mode configuration
(defun elisp-config ()
  "Configuration for elisp-mode."
  (default-programming-config)
  )

(add-hook 'emacs-lisp-mode-hook 'elisp-config)

(defun json-mode-config ()
  "Configuration for JSON-mode."
  (rainbow-delimiters-mode)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (window-margin-mode)
  )

(defun my-javascript-mode-hook ()
  "Configuration for javascript."
  (default-programming-config)
  (setq js-indent-level 2)
  )

(add-hook 'javascript-mode-hook 'my-javascript-mode-hook)

;; Python Mode
(defun my-python-mode-hook ()
  "Configure settings for python."
  (default-programming-config)
  (highlight-indentation-mode)
  (set-face-background 'highlight-indentation-face "#444466")
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)


(add-hook 'json-mode-hook 'json-mode-config)

(defun my-markdown-mode-hook ()
  "Add some nice extensions for dealing with various markdown modes."

  (defun hugo-extras ()
    "Add a bunch of extra functions for hugo-specific markdown."

    (defun insert-relative-link (name to)
      "Inserts a relative link called NAME to the section named TO."
      (insert (format "[%s]({{<relref \"#%s\">}})" name to))
      )

    (defun rel-link ()
      "Query the user for a link name and section heading, then insert a
      relative link."
      (interactive)
      (let ((name (read-string "Link Name: ")))
        (let ((to (read-string "Link To: ")))
          (insert-relative-link name to9)
          )
        )
      )
    )
  )

(add-hook 'markdown-mode-hook 'default-programming-config)
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;; add sql-indent when loading sql files
(eval-after-load "sql"
  '(load-library "sql-indent"))


(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo `find-tag'."
  "If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run `etags' on all peer files in current dir and reload them silentlyf, \
if EXTENSION is specified, use it for refreshing etags, or default to .el."

  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(defun create-tags(format)
  (eshell-command
   (format "find %s -type f -name \"%s\" | etags -" (pwd) format)
   )
  )

(defalias 'list-buffers 'ibuffer)

;; TeX Mode
(defun beamer-utils()
  (interactive)
  (defun beamer-new-frame(name)
    (insert "\\begin{frame}")
    (reindent-then-newline-and-indent)
    (insert "\\frametitle{")
    (insert name)
    (insert "}")
    (reindent-then-newline-and-indent)
    (insert "\\end{frame}")
    (reindent-then-newline-and-indent)
    (previous-line)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun new-slide()
    "Get a slide NAME and insert it."
    (interactive)
    (let ((name (read-string "Frame Title: ")))
      (beamer-new-frame name))
    )

  (local-set-key (kbd "C-c f") 'new-slide)

  (defun simplified-block()
    (interactive)
    (insert "\\begin{exampleblock}{In Plain English}")
    (reindent-then-newline-and-indent)
    (insert "\\end{exampleblock}")
    (reindent-then-newline-and-indent)
    (previous-line)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )

  (setq org-latex-listings 'minted)
  (setq org-latex-custom-lang-environments
        '(
          (emacs-lisp "common-lispcode")
          )
        )
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("linenos" "false")))

  (setq org-latex-to-pdf-process
        '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))

  (local-set-key (kbd "C-c s") 'simplified-block)
  )

;; AUCTeX-mode
(setq TeX-parse-self t); Enable automatic parsing
(setq TeX-auto-save t); Enable parse on save

(defun extra-cc-keybindings()
  (local-set-key (kbd "C-?") (kbd "M-x manual-entry RET"))
  )

;; Cc Mode
;; Set the indentation to 4 spaces
(setq-default c-basic-offset 2
              c-default-style "bsd")

;; Enable 80-column fill indicator for C files
(add-hook 'c-mode-hook 'turn-on-auto-fill)

;; set up auto-complete-mode for C files
(add-hook 'c-mode-hook 'auto-complete-mode)
(add-hook 'c-mode-hook 'extra-cc-keybindings)

(defcustom haskell-pretty-printer nil
  "Program used to reformat haskell source code."
  :group 'haskell-config
  :type '(choice (const "stylish-haskell")
                 (const "fourmolu")
                 (const "ormolu")
                 (const "brittany")
                 (const nil)
                 (string :tag "other formatter")))

(defcustom haskell-format-on-save nil
  "If enabled, format haskell buffer on save."
  :group 'haskell-config
  :type '(boolean))

(defcustom cabal-pretty-printer nil
  "Program used to reformat cabal configurations."
  :group 'haskell-config
  :type '(choice (const "stylish-cabal")
                 (const nil)
                 (string :tag "other formatter")))

(defcustom cabal-format-on-save nil
  "If enabled, format cabal buffer on safe."
  :group 'haskell-config
  :type '(boolean))

(defun pretty-print-buffer(format-command)
  "Run FORMAT-COMMAND to pretty-print the current buffer."
  ((defvar-local p (point))
    (shell-command-on-region (point-min) (point-max) format-command nil t)
    (goto-char p)
    )
  )

(defun haskell-pretty-print-buffer()
  "Pretty-print a haskell buffer using haskell-pretty-printer."
  (interactive)
  (when haskell-pretty-printer (pretty-print-buffer haskell-pretty-printer))
  )

(defun cabal-pretty-print-buffer()
  "Pretty-print a cabal buffer using cabal-pretty-printer."
  (interactive)
  ((when cabal-pretty-printer (pretty-print-buffer cabal-pretty-printer)))
  )

(defun haskell-config-save-hook()
  "Save hook function will automatically format a haskell or cabal buffer on save."
  (if
      (and (eq major-mode 'haskell-mode) (haskell-format-on-save))
      (haskell-pretty-print-buffer)
    (if
        (and (eq major-mode 'haskell-cabal-mode) (cabal-format-on-save))
        (cabal-pretty-print-buffer))))


(defun haskell-config-setup-haskell-mode()
  "Setup the haskell editing environment."

  (setq haskell-tags-on-save nil)
  (rainbow-delimiters-mode t)
  (turn-on-line-numbers)

  (local-set-key (kbd "C-)") 'forward-sexp)
  (local-set-key (kbd "C-(") 'backward-sexp)
  (local-set-key (kbd "C-<tab>") 'haskell-pretty-print-buffer)
  (local-set-key (kbd "M-.") 'haskell-mode-tag-find)

  (custom-set-variables
    '(haskell-process-suggest-remove-import-lines t)
    '(haskell-process-auto-import-loaded-modules t)
    '(haskell-process-log t))

  (eval-after-load 'haskell-mode '(progn
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))

  (eval-after-load 'haskell-cabal '(progn
    (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

  )

;(add-hook 'haskell-mode-hook 'nix-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-config-setup-haskell-mode)

(defun haskell-config-setup-cabal-mode()
  "Setup the cabal editing environment."
  (local-set-key (kbd "C-)") 'forward-sexp)
  (local-set-key (kbd "C-(") 'backward-sexp)
  (local-set-key (kbd "C-<tab>") 'cabal-pretty-print-buffer)
  )

(add-hook 'haskell-cabal-mode-hook 'haskell-config-setup-cabal-mode)
(add-hook 'before-save-hook 'haskell-config-save-hook)



;;; init.el ends here
