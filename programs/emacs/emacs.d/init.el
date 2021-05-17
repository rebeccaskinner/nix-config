;;; init --- Emacs configuration
;;; provide (init)
;;; Commentary:

;;; Code:
;; Disable the splash screen
(setq inhibit-splash-screen t)


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
  )

(setup-global-keybindings)

(defun configure-temp-files()
  "Set the auto-save and backup files to /tmp/"
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  )

(configure-temp-files)

(setq-default indent-tabs-mode nil)

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
  (setq org-start-with-inline-images t)
  )

(defun enable-orgmode-ruby-execution()
  "Enable ruby rendering and execution with org-bable."
  (require 'ob-ruby)
  )

(defun enable-org-reveal()
  (require 'ox-reveal)
  (setq org-reveal-root (make-home-path "projects/reveal.js"))
  )

(add-hook 'org-mode-hook 'enable-org-reveal)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'enable-orgmode-graphiz-execution)
(add-hook 'org-mode-hook 'enable-orgmode-ditaa-execution)
(add-hook 'org-mode-hook 'enable-orgmode-inline-preview)
(add-hook 'org-mode-hook 'enable-orgmode-ruby-execution)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Nerd" :foundry "ADBO" :slant normal :weight semi-bold :heightf 140 :width normal :height 102))))
 )


(defun configure-look-and-feel ()
  "Run some stuff after init, like setting a theme and disabling scrollbars."
  ;; Setup theme
  (load-theme 'darkplum t)

  ;; disable the menu bar
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  )

(defun deamon-look-and-feel (frame)
  "Wrapper to run look-and-feel per-frame with emacsclient."
  (select-frame frame)
  (configure-look-and-feel)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions #'deamon-look-and-feel)
    (configure-look-and-feel)
  )


;; Rainbow Delimiters
(require 'rainbow-delimiters)

;;; Setup Fill-Mode
(require 'fill-column-indicator)

;; Visual fci config
(setq fci-rule-width 1)
(setq fci-rule-color "darkgrey")

;; Turn on fci mode by default
(add-hook 'after-init-hook 'fci-mode)

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

;; mode specific configs
(defun default-programming-config ()
  "Configure some sane defaults shared across various programming-related major modes."
  (auto-fill-mode 1)
  (auto-complete-mode 1)
  (rainbow-delimiters-mode 1)
  (fci-mode 1)
  (set-fill-column 80)
  (enable-expand-region)
;  (line-nums)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq tab-width 2)
  (global-set-key (kbd "C-)") 'forward-sexp)
  (global-set-key (kbd "C-(") 'backward-sexp)
  )

(defun my-dhall-mode-config ()
  "Configure basic settings when editing in dhall-mode."
  (default-programming-config)
  )

(add-hook 'dhall-mode 'my-dhall-mode-config)

(defun my-nix-mode-config ()
  "Configure defaults for editing nix expressions."
  (default-programming-config)
  )

(add-hook 'nix-mode 'my-nix-mode-config)

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

  (defun add-code-block ()
    "Add a code block without spawning a mini-window."
    (interactive)
    (let ((lang (read-string "language: " nil 'code-block-history)))
      (add-to-history 'code-block-history lang)
      (insert-code-block-without-contents lang)
      )
    )

  (global-set-key (kbd "C-\"") "“")
  (global-set-key (kbd "M-\"") "”")
  (global-set-key (kbd "C-c t") 'make-tag)
  (global-set-key (kbd "C-c b") 'add-code-block)
  (global-set-key (kbd "C-c m") 'add-objc-method)
  )

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

;; Configure go for eshell
;; Set GOPATH in the emacs environment
(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOROOT" "/usr/local/go")

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

;; Go Mode
(defun my-go-mode-hook ()
  "Use goimports instead of go-fmt."
  (default-programming-config)
  (soft-wrap-config)

  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (local-set-key (kbd "C-<tab>") 'gofmt-before-save)

  ;; Add $GOPATH/bin/ to the search path for executable binaries
  (add-to-paths (make-home-path "go/bin"))

  (local-set-key (kbd "M-.") 'godef-jump)

  ;; setup autocomplete for go
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)

  ;; Set the tab-width to something reasonable
  (setq tab-width 2)

  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ; Use C-<return> to go-run the current file outside of go-playground files
  (local-set-key (kbd "C-<return>") 'go-run)

  ; Use 'C-Shift-<return>' to run tests on the current file
  (local-set-key (kbd "C-S-<return>") 'go-test-current-file)

  (defun go-if-err()
    "Insert golang error handling boilerplate."
    (interactive)
    (insert "if err != nil { return nil, err }")
    (reindent-then-newline-and-indent)
    )

  (defun go-mode-new-test(name)
    "Create a new test called NAME."
    (insert (format "// %s runs a test" name))
    (reindent-then-newline-and-indent)
    (insert (format "func %s (t *testing.T) {" name))
    (reindent-then-newline-and-indent)
    (insert "t.Parallel()")
    (reindent-then-newline-and-indent)
    (reindent-then-newline-and-indent)
    (insert "}")
    (reindent-then-newline-and-indent)
    (previous-line)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun go-mode-new-subtest(description)
    "Use t.Run to create a new subtest with DESCRIPTION."
    (insert (format "t.Run(\"%s\", func (t *testing.T) {" description))
    (reindent-then-newline-and-indent)
    (insert "t.Parallel()")
    (reindent-then-newline-and-indent)
    (reindent-then-newline-and-indent)
    (insert "})")
    (reindent-then-newline-and-indent)
    (previous-line)
    (previous-line)
    (end-of-line)
    (newline-and-indent)
    )

  (defun new-test()
    "Get a test name and insert it."
    (interactive)
    (let ((name (read-string "Test Name: ")))
      (go-mode-new-test name))
    )

  (defun new-sub-test()
    "Get a subtest description and insert it."
    (interactive)
    (let ((desc (read-string "Description: ")))
      (go-mode-new-subtest desc))
    )

  (global-set-key (kbd "C-c t") 'new-test)
  (global-set-key (kbd "C-c r") 'new-sub-test)
  (global-set-key (kbd "C-c e") 'go-if-err)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-playground-mode 'my-go-mode-hook)
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

(defun my-haskell-mode-hooks()
  (defun hsfmt()
    "Apply stylish-haskell to the current buffer."
    (interactive)
    (defvar-local p (point))
    (shell-command-on-region (point-min) (point-max)  "stylish-haskell" nil t)
    (goto-char p)
    )
  (defun hs-save-hook()
    (when (eq major-mode 'haskell-mode)
      (hsfmt)
      )
    )
  (global-set-key (kbd "C-<tab>") 'hsfmt)
  )

;; Haskell Mode
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  )

(add-hook 'haskell-mode-hook 'my-haskell-mode-hooks)

;; Setup haskell-cabal-mode
(defun my-haskell-cabal-mode()
  (defun hsfmt()
    "Apply stylish-cabal to the current buffer."
    (interactive)
    (defvar-local p (point))
    (shell-command-on-region (point-min) (point-max)  "stylish-cabal" nil t)
    (goto-char p)
    )
  (defun hs-save-hook()
    (when (eq major-mode 'haskell-mode)
      (hsfmt)
      )
    )
  (global-set-key (kbd "C-<tab>") 'hsfmt)
  )
(add-hook 'haskell-cabal-mode-hook 'my-haskell-cabal-mode)

(defun my-rust-mode-hook()
  (defun rsfmt()
    "Call rustfmt."
    (interactive)
    (setq p (point))
    (shell-command-on-region (point-min) (point-max)  "rustfmt" nil t)
    (goto-char p)
    )
  (defun rust-save-hook()
    (when (eq major-mode 'rust-mode)
      (rsfmt)
      )
    )
  (add-hook 'before-save-hook 'rust-save-hook)
  (local-set-key (kbd "C-<tab>") 'rsfmt)
  (default-programming-config)
  (setq rust-indent-offset 2)
  )

(add-hook 'rust-mode-hook 'my-rust-mode-hook)

;; TeX Mode
(defun beamer-utils()
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
(global-set-key (kbd "C-c f") 'new-slide)

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
(global-set-key (kbd "C-c s") 'simplified-block)
)

;; AUCTeX-mode
(setq TeX-parse-self t); Enable automatic parsing
(setq TeX-auto-save t); Enable parse on save

(defun extra-cc-keybindings()
  (global-set-key (kbd "C-?") (kbd "M-x manual-entry RET"))
  )

;; Cc Mode
;; Set the indentation to 4 spaces
(setq-default c-basic-offset 2
              c-default-style "bsd")

;; Enable 80-column fill indicator for C files
(add-hook 'cc-mode-hook 'turn-on-auto-fill)

;; set up auto-complete-mode for C files
(add-hook 'cc-mode-hook 'auto-complete-mode)
(add-hook 'cc-mode-hook 'etags-c-tags)
(add-hook 'cc-mode-hook 'auto-complete-mode)
(add-hook 'cc-mode-hook 'extra-cc-keybindings)

(pdf-loader-install)

;;; init.el ends here