;;; init --- Emacs configuration
;;; provide (init)
;;; Commentary:

;;; Code:
;; Disable the splash screen
(setq inhibit-splash-screen t)

(require 'pml-mode)

(defun configure-look-and-feel ()
  "Configure theme, font, and chrome. Safe to call per-frame under the daemon."
  (load-theme 'darkplum t)
  (set-face-attribute 'default nil :family "FiraCode" :foundry "ADBO" :height 130)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

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

;; -------------------------------------------------------------------
;; 🧠 gptel Keybindings (for code + Org mode buffers)
;;
;; C-c g g   →  Open a new gptel chat buffer for the current context
;; C-c g s   →  Send a prompt at point (or from minibuffer) to LLM
;; C-c g r   →  Send the active region (or Org subtree) to LLM
;; C-c g b   →  Interactively choose/switch gptel backend (OpenAI / Claude)
;; C-c g t   →  Open gptel-transient menu (adjust model, temperature, etc.)
;;
;; Notes:
;; - Works in programming modes and inside Org-mode source blocks.
;; - Backend defaults to OpenAI (gpt-4o). Use C-c g b to switch to Claude.
;; - Make sure OPENAI_API_KEY and ANTHROPIC_API_KEY are set in your environment.
;; -------------------------------------------------------------------

(with-eval-after-load 'org-ai
  (setq org-ai-openai-api-token (getenv "OPENAI_API_KEY")))

;; Make sure this is set so gptel never prompts
(setq gptel-api-key (getenv "OPENAI_API_KEY"))

;; --- LLMs in Emacs with gptel + Org + OpenAI + Claude ---

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-send-region gptel-fn-complete gptel-set-backend)
  :init
  ;; Tweak display; put chat buffers at bottom
  (setq gptel-display-buffer-action '(display-buffer-at-bottom))
  :bind (("C-c g g" . gptel)               ;; open chat buffer for current file
         ("C-c g s" . gptel-send)          ;; send prompt at point / minibuffer
         ("C-c g a" . gptel-add)           ;; add the active region to gptel's context

         ("C-c g f" . gptel-fn-complete)   ;; complete current function
         ("C-c g b" . my/gptel-choose-backend)) ;; quickly switch backends
  :config
  ;; --- Define backends ---
  (setq my/gptel-openai
        (gptel-make-openai "openai"
          :key   (getenv "OPENAI_API_KEY")
          :host  "api.openai.com"
          :endpoint "/v1/chat/completions"
          :models '("gpt-4o" "gpt-4o-mini" "gpt-5" "gpt-5-mini")))

  (setq my/gptel-claude
        (gptel-make-anthropic "claude"
          :key   (getenv "ANTHROPIC_API_KEY")
          ;; Use any current Claude chat-completion model you prefer:
          :models '("claude-opus-4-7" "claude-sonnet-4-6" "claude-haiku-4-5-20251001")))

  (setq gptel-backends `((openai . ,my/gptel-openai)
                         (claude . ,my/gptel-claude)))

  ;; Default: OpenAI
  (setq gptel-backend (alist-get 'openai gptel-backends))

  ;; ;; Helper to switch backends quickly
  (defun my/gptel-choose-backend ()
    "Interactively choose a gptel backend (OpenAI/Claude)."
    (interactive)
    (let* ((choice (intern (completing-read "gptel backend: "
                                            (mapcar #'car gptel-backends) nil t)))
           (backend (alist-get choice gptel-backends)))
      (gptel-set-backend backend)
      (message "gptel backend set to %s" choice)))

  ;; --- Org-mode niceties ---
  ;; Use the same keybindings inside Org buffers
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c g g") #'gptel)
    (define-key org-mode-map (kbd "C-c g r") #'gptel-send-region)
    (define-key org-mode-map (kbd "C-c g s") #'gptel-send))
)

;; Optional: a quick transient UI for switching models/params
;; (use-package gptel-transient
;;   :ensure t
;;   :after gptel
;;   :bind (("C-c g t" . gptel-transient)))

;; Optional: async HTTP client gptel can use if available
(use-package plz
  :ensure t
  :defer t)

;; Tip: set your API keys in your env (e.g., ~/.profile or shell rc)
;; export OPENAI_API_KEY="sk-..."
;; export ANTHROPIC_API_KEY="sk-ant-..."


;; Global keybindings
(global-set-key (kbd "M-P") 'ace-window)
(global-set-key (kbd "<M-up>") 'ace-window)
(global-set-key (kbd "C-'") 'goto-last-change)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-\"") "“")
(global-set-key (kbd "M-\"") "”")

;; Send auto-save and backup files to /tmp instead of cluttering source dirs
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil)

(use-package ivy
  :ensure t
  :config
  (progn
    (ivy-mode)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t))
)

;; Turn on visual line-wrapping mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'tex-mode-hook 'turn-on-visual-line-mode)

(setq org-adapt-indentation nil)

;; Org agenda
(setq org-agenda-files (list "~/agenda.org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; flycheck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Use built-in fill-column-indicator mode
(setq-default fill-column 80)  ;; Set your desired fill column width
(setq display-fill-column-indicator-character ?\u2502)  ;; Set the character if you want a custom one

(use-package display-fill-column-indicator
  :ensure nil  ;; Built-in, no need to install
  :hook (after-init . global-display-fill-column-indicator-mode)  ;; Enable globally
  :config
  ;; Set the color and width (using face attributes)
  (set-face-foreground 'fill-column-indicator "darkgrey")
  (setq-default display-fill-column-indicator nil))  ;; Turn it off by default for modes that need it explicitly

;; Line numbers
(defun absolute-line-numbers ()
  (interactive)
  (setq display-line-numbers-type t)
  (display-line-numbers-mode))

(defun relative-line-numbers ()
  (interactive)
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode))

(defun visual-line-numbers ()
  (interactive)
  (setq display-line-numbers-type 'visual)
  (display-line-numbers-mode))

(defun turn-off-line-numbers () (interactive) (display-line-numbers-mode -1))
(defun turn-on-line-numbers () (interactive) (display-line-numbers-mode 1))

(defun toggle-line-numbers ()
  (interactive)
  (if (eq display-line-numbers nil)
      (turn-on-line-numbers)
    (turn-off-line-numbers)))

(global-set-key (kbd "C-c n") 'toggle-line-numbers)

(use-package expand-region
     :ensure t
     :bind (("C-=" . er/expand-region)))

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
  (turn-on-line-numbers))

(defun my-dhall-mode-config ()
  "Configure basic settings when editing in dhall-mode."
  (default-programming-config)
  )

(add-hook 'dhall-mode-hook 'my-dhall-mode-config)

;; Extra functions for pml mode
;; PML editing helpers
(defvar pml/tag-name-history '())
(defvar pml/tag-contents-history '())
(defvar pml/code-block-history '())
(defvar pml/inline-code-history '())

(defun pml/insert-tag-with-value (tag val)
  (insert (format "<%s>%s</%s>" tag val tag)))

(defun pml/make-tag ()
  "Read a tag name and contents from the minibuffer, then insert the tag."
  (interactive)
  (let ((tag (read-string "tag: " nil 'pml/tag-name-history)))
    (add-to-history 'pml/tag-name-history tag)
    (let ((contents (read-string "contents: " nil 'pml/tag-contents-history)))
      (add-to-history 'pml/tag-contents-history contents)
      (pml/insert-tag-with-value tag contents))))

(defun pml/insert-code-block-without-contents (lang)
  (insert (format "{:language=\"%s\"}" lang))
  (newline-and-indent)
  (insert "~~~")
  (newline-and-indent)
  (insert "~~~")
  (forward-line -1)
  (end-of-line)
  (newline-and-indent))

(defun pml/insert-code-block-with-contents (lang contents)
  (pml/insert-code-block-without-contents lang)
  (insert contents)
  (forward-line 1)
  (end-of-line)
  (newline-and-indent))

(defun pml/add-backtick-code ()
  "Add some inline code using backticks."
  (interactive)
  (let ((code (read-string "code: " nil 'pml/inline-code-history)))
    (insert (format "`%s`" code))))

(defun pml/add-code-block ()
  "Add a code block without spawning a mini-window."
  (interactive)
  (let ((lang (read-string "language: " nil 'pml/code-block-history)))
    (add-to-history 'pml/code-block-history lang)
    (pml/insert-code-block-without-contents lang)))

(defun pml/insert-lambda ()
  "Insert a literal lambda character."
  (interactive)
  (insert "λ"))

(defun pml-mode-tools ()
  "Bind PML editing helpers in the current buffer."
  (interactive)
  (local-set-key (kbd "C-c l") 'pml/insert-lambda)
  (local-set-key (kbd "C-c t") 'pml/make-tag)
  (local-set-key (kbd "C-c b") 'pml/add-code-block)
  (local-set-key (kbd "C-c m") 'pml/add-backtick-code))

;; Markdown editing helpers (for code-focused blog posts)
(defvar markdown/tag-name-history '())
(defvar markdown/tag-contents-history '())
(defvar markdown/code-block-history '())
(defvar markdown/inline-code-history '())

(defun markdown/insert-tag-with-value (tag val)
  (insert (format "<%s>%s</%s>" tag val tag)))

(defun markdown/make-tag ()
  "Read a tag name and contents from the minibuffer, then insert the tag."
  (interactive)
  (let ((tag (read-string "tag: " nil 'markdown/tag-name-history)))
    (add-to-history 'markdown/tag-name-history tag)
    (let ((contents (read-string "contents: " nil 'markdown/tag-contents-history)))
      (add-to-history 'markdown/tag-contents-history contents)
      (markdown/insert-tag-with-value tag contents))))

(defun markdown/add-inline-code ()
  "Read some code and insert it wrapped in backticks."
  (interactive)
  (let ((code (read-string "code: " nil 'markdown/inline-code-history)))
    (add-to-history 'markdown/inline-code-history code)
    (insert (format "`%s`" code))))

(defun markdown/insert-code-block-without-contents (lang)
  (insert (format "```%s" lang))
  (newline-and-indent)
  (insert "```")
  (forward-line -1)
  (end-of-line)
  (newline-and-indent))

(defun markdown/insert-code-block-with-contents (lang contents)
  (markdown/insert-code-block-without-contents lang)
  (insert contents)
  (forward-line 1)
  (end-of-line)
  (newline-and-indent))

(defun markdown/add-code-block ()
  "Add a code block without spawning a mini-window."
  (interactive)
  (let ((lang (read-string "language: " nil 'markdown/code-block-history)))
    (add-to-history 'markdown/code-block-history lang)
    (markdown/insert-code-block-without-contents lang)))

(defun markdown-mode-tools ()
  "Bind markdown editing helpers in the current buffer."
  (local-set-key (kbd "C-c t") 'markdown/make-tag)
  (local-set-key (kbd "C-c b") 'markdown/add-code-block)
  (local-set-key (kbd "C-c m") 'markdown/add-inline-code))

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

;; Hugo-specific markdown helpers
(defun markdown/insert-relative-link (name to)
  "Insert a relative link called NAME to the section named TO."
  (insert (format "[%s]({{<relref \"#%s\">}})" name to)))

(defun markdown/rel-link ()
  "Query the user for a link name and section heading, then insert a relative link."
  (interactive)
  (let ((name (read-string "Link Name: "))
        (to (read-string "Link To: ")))
    (markdown/insert-relative-link name to)))

(add-hook 'markdown-mode-hook 'default-programming-config)

;; add sql-indent when loading sql files
(eval-after-load "sql"
  '(load-library "sql-indent"))


(defun my/find-tag-refresh-advice (orig-fn &rest args)
  "Around advice for `find-tag': rerun etags and retry if the tag is not found.
If the buffer is modified, ask to save before refreshing."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case _
        (apply orig-fn args)
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             (apply orig-fn args)))))

(advice-add 'find-tag :around #'my/find-tag-refresh-advice)

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
;; Beamer (LaTeX presentation) helpers
(defun beamer/new-frame (name)
  "Insert a Beamer frame with title NAME and leave point inside."
  (insert "\\begin{frame}")
  (reindent-then-newline-and-indent)
  (insert "\\frametitle{")
  (insert name)
  (insert "}")
  (reindent-then-newline-and-indent)
  (insert "\\end{frame}")
  (reindent-then-newline-and-indent)
  (forward-line -2)
  (end-of-line)
  (newline-and-indent))

(defun beamer/new-slide ()
  "Prompt for a frame title and insert a new Beamer frame."
  (interactive)
  (let ((name (read-string "Frame Title: ")))
    (beamer/new-frame name)))

(defun beamer/simplified-block ()
  "Insert a Beamer exampleblock titled \"In Plain English\"."
  (interactive)
  (insert "\\begin{exampleblock}{In Plain English}")
  (reindent-then-newline-and-indent)
  (insert "\\end{exampleblock}")
  (reindent-then-newline-and-indent)
  (forward-line -2)
  (end-of-line)
  (newline-and-indent))

(defun beamer-utils ()
  "Configure org-latex export options and bind Beamer helpers in the current buffer."
  (interactive)
  (setq org-latex-listings 'minted)
  (setq org-latex-custom-lang-environments
        '((emacs-lisp "common-lispcode")))
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\scriptsize")
          ("linenos" "false")))
  (setq org-latex-pdf-process
        '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  (local-set-key (kbd "C-c f") 'beamer/new-slide)
  (local-set-key (kbd "C-c s") 'beamer/simplified-block))

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

(defun pretty-print-buffer (format-command)
  "Run FORMAT-COMMAND to pretty-print the current buffer."
  (let ((p (point)))
    (shell-command-on-region (point-min) (point-max) format-command nil t)
    (goto-char p)))

(defun haskell-pretty-print-buffer()
  "Pretty-print a haskell buffer using haskell-pretty-printer."
  (interactive)
  (when haskell-pretty-printer (pretty-print-buffer haskell-pretty-printer))
  )

(defun cabal-pretty-print-buffer()
  "Pretty-print a cabal buffer using cabal-pretty-printer."
  (interactive)
  (when cabal-pretty-printer (pretty-print-buffer cabal-pretty-printer)))

(defun haskell-config-save-hook()
  "Save hook function will automatically format a haskell or cabal buffer on save."
  (cond
   ((and (eq major-mode 'haskell-mode) haskell-format-on-save)
    (haskell-pretty-print-buffer))
   ((and (eq major-mode 'haskell-cabal-mode) cabal-format-on-save)
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

  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)

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
