;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; provide (init)
;;; Commentary:

;;; Code:
;; Disable the splash screen
(setq inhibit-splash-screen t)

(require 'pml-mode)
(require 'persistent-mode)

;; Evil.  evil-collection needs both of these set before evil loads.
(setq evil-want-integration t
      evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
;; Consistent vim keys in magit, dired, help, org-agenda, and the other
;; special-mode buffers.
(require 'evil-collection)
(evil-collection-init)
;; Org-specific keys (heading motion, table navigation, agenda).
(with-eval-after-load 'org
  (require 'evil-org)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Load the theme once; it applies to every later frame, including
;; frames the daemon creates.  Per-frame work is limited to font and
;; chrome below.
(load-theme 'darkplum t)

(defun configure-look-and-feel ()
  "Configure font and chrome.  Safe to call per-frame under the daemon."
  (set-face-attribute 'default nil :family "FiraCode" :foundry "ADBO" :height 130)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

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
;; The C-c g bindings are documented in cheatsheets/gptel.txt; view it
;; with `:cheatsheet gptel' (or M-x cheatsheet).  Keep that file in sync
;; with the :bind form below.
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
  :commands (gptel gptel-send gptel-add)
  :init
  ;; Tweak display; put chat buffers at bottom
  (setq gptel-display-buffer-action '(display-buffer-at-bottom))
  ;; These are global, so they also apply in org and prog-mode buffers.
  :bind (("C-c g g" . gptel)               ;; open chat buffer for current file
         ("C-c g s" . gptel-send)          ;; send prompt at point (or region)
         ("C-c g a" . gptel-add)           ;; add the active region to gptel's context
         ("C-c g b" . my/gptel-choose-backend)) ;; quickly switch backends
  :config
  ;; --- Define backends ---
  (setq my/gptel-openai
        (gptel-make-openai "openai"
          :key   (getenv "OPENAI_API_KEY")
          :host  "api.openai.com"
          :endpoint "/v1/chat/completions"
          :models '("gpt-4o" "gpt-4o-mini" "gpt-5" "gpt-5-mini")))

  ;; No :models here: gptel's built-in list for this backend tracks
  ;; current Claude releases, so it stays fresh with package updates.
  (setq my/gptel-claude
        (gptel-make-anthropic "claude"
          :key   (getenv "ANTHROPIC_API_KEY")))

  (setq gptel-backends `((openai . ,my/gptel-openai)
                         (claude . ,my/gptel-claude)))

  ;; Default: OpenAI
  (setq gptel-backend (alist-get 'openai gptel-backends))

  ;; Helper to switch backends quickly.  gptel has no setter for this;
  ;; the backend and model are plain variables, and the model must be
  ;; one the new backend knows about, so reset it to that backend's
  ;; first model.
  (defun my/gptel-choose-backend ()
    "Interactively choose a gptel backend (OpenAI/Claude)."
    (interactive)
    (let* ((choice (intern (completing-read "gptel backend: "
                                            (mapcar #'car gptel-backends) nil t)))
           (backend (alist-get choice gptel-backends)))
      (setq gptel-backend backend
            gptel-model (car (gptel-backend-models backend)))
      (message "gptel backend set to %s (%s)" choice gptel-model))))

;; Optional: a quick transient UI for switching models/params
;; (use-package gptel-transient
;;   :ensure t
;;   :after gptel
;;   :bind (("C-c g t" . gptel-transient)))

;; Optional: async HTTP client gptel can use if available
(use-package plz
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

;; Keep Customize output out of init.el, which is a read-only symlink
;; into the Nix store.  ~/.emacs.d itself is a real, writable directory.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Built-in conveniences
(electric-pair-mode -1)        ; no auto-inserted closing brackets or quotes, anywhere
(which-key-mode 1)             ; show the completions of C-c, C-c g, ... as you type
(savehist-mode 1)              ; minibuffer histories (code-block languages, etc.) survive restarts
(recentf-mode 1)               ; recently visited files
(global-auto-revert-mode 1)    ; pick up changes made outside emacs (git, formatters)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; -------------------------------------------------------------------
;; Completion.  Minibuffer: vertico + orderless + marginalia + consult
;; + embark.  In-buffer: corfu + cape.  Keys are in :cheatsheet completion.
;; -------------------------------------------------------------------
(setq enable-recursive-minibuffers t)   ; embark/consult open a minibuffer from the minibuffer

;; Minibuffer UI.  savehist-mode (above) is what lets vertico sort by history.
(require 'vertico)
(require 'vertico-directory)
(vertico-mode 1)
;; Same as ivy's C-M-j: accept what I typed rather than the highlighted match.
(define-key vertico-map (kbd "C-M-j") #'vertico-exit-input)
;; Directory navigation like ivy: RET enters a directory, DEL backs up a component.
(define-key vertico-map (kbd "RET") #'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;; Matching: space-separated terms in any order; `!term' excludes.
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; Docstrings next to M-x commands, details next to files and buffers.
(marginalia-mode 1)

;; consult: better versions of a few built-in commands.
(require 'consult)
(setq consult-narrow-key "<")                     ; e.g. "< f" in consult-buffer shows only files
(global-set-key (kbd "C-x b") #'consult-buffer)   ; buffers + recent files + bookmarks (ivy's virtual buffers)
(global-set-key (kbd "M-g o") #'consult-outline)  ; jump to a heading (markdown, org) or top-level form
(global-set-key (kbd "M-g i") #'consult-imenu)    ; jump to a definition
(global-set-key (kbd "M-s r") #'consult-ripgrep)  ; grep the project

;; embark: act on the candidate or the thing at point.
(require 'embark)
(require 'embark-consult)
(global-set-key (kbd "M-o") #'embark-act)                          ; like ivy's M-o
(define-key minibuffer-local-map (kbd "C-c C-o") #'embark-export)  ; like ivy-occur: results to a buffer

;; In-buffer popup completion.
;; evil-collection key themes for the popup, read when corfu loads:
;;   tab-n-go      TAB / S-TAB cycle candidates (as auto-complete did); nothing is
;;                 preselected, so typing on just keeps typing
;;   magic-return  RET inserts the candidate only if you navigated to one;
;;                 otherwise it is an ordinary newline
(defvar evil-collection-corfu-key-themes)   ; defined by evil-collection-corfu, loaded with corfu
(setq evil-collection-corfu-key-themes '(default tab-n-go magic-return))
(require 'corfu)
(require 'corfu-auto)
(setq corfu-auto nil          ; popup only on request (C-M-i); M-x corfu-auto-toggle turns auto on
      corfu-auto-prefix 2     ; settings for when auto is on
      corfu-auto-delay 0.1
      corfu-preselect 'prompt ; tab-n-go sets this too; stated here so it's not a surprise
      corfu-cycle t
      ;; Haskell buffers stay popup-free; :cheatsheet haskell-mode says how to turn it on.
      global-corfu-modes '((not haskell-mode haskell-cabal-mode haskell-interactive-mode) t))
(global-corfu-mode 1)

(defun corfu-auto-toggle ()
  "Turn corfu's automatic popup on or off in every buffer.
corfu reads `corfu-auto' only when `corfu-mode' starts in a buffer, so
setting the variable is not enough: restart the mode where it is on."
  (interactive)
  (setq corfu-auto (not corfu-auto))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when corfu-mode
        (corfu-mode -1)
        (corfu-mode 1))))
  (message "corfu auto-complete %s" (if corfu-auto "on" "off")))
;; Emacs 31 can draw the popup in terminal frames natively.  Older Emacs
;; (the Mac host runs 30.x) cannot, so there corfu-terminal draws an
;; overlay popup in tty frames and steps aside in GUI frames, which
;; makes it safe to enable globally under the daemon.
(when (< emacs-major-version 31)
  (corfu-terminal-mode 1))
;; Completion sources for modes that provide none of their own: words
;; from other buffers (what auto-complete gave us) and file paths.
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(setq dabbrev-case-replace nil)

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
  :hook (after-init . global-flycheck-mode))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Use built-in fill-column-indicator mode
(setq-default fill-column 80)  ;; Set your desired fill column width
(setq display-fill-column-indicator-character ?\u2502)  ;; Set the character if you want a custom one

(use-package display-fill-column-indicator
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
     :bind (("C-=" . er/expand-region)))

;; mode specific configs
(defun default-programming-config ()
  "Configure some sane defaults shared across various programming-related major modes."
  (set-fill-column 80)
  (auto-fill-mode 1)
  (rainbow-delimiters-mode 1)
  ;; Buffer-local: a global hook here would strip trailing whitespace
  ;; from every buffer, including markdown's two-space line breaks.
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (setq tab-width 2)
  (local-set-key (kbd "C-)") 'forward-sexp)
  (local-set-key (kbd "C-(") 'backward-sexp)
  (turn-on-line-numbers))

(defun my-dhall-mode-config ()
  "Configure basic settings when editing in dhall-mode."
  (default-programming-config)
  )

(add-hook 'dhall-mode-hook 'my-dhall-mode-config)

;; Markdown editing helpers (for code-focused blog posts)
;; Highlight fenced code blocks with the named language's major mode.
;; C-c ' (markdown-edit-code-block) opens the block at point in a buffer
;; in that mode, like org's org-edit-special; it needs edit-indirect,
;; which the package list installs.  C-c C-c there writes the block back.
(setq markdown-fontify-code-blocks-natively t)

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

;; Org editing helpers (org-mode equivalents of the pml/markdown helpers above)
;;
;; Org already provides most of this natively:
;;   C-c C-,       org-insert-structure-template  (prompts for block type, wraps region)
;;   C-c C-x C-f   org-emphasize                  (prompts for marker, wraps region)
;;   C-c '         org-edit-special               (edit a src block in its native mode)
;; These wrappers just pre-fill "src <lang>" / "~" and keep a minibuffer
;; history, so the bindings match the pml and markdown ones.
(defvar org/code-block-history '())
(defvar org/inline-code-history '())

(defun org/add-code-block ()
  "Insert a #+begin_src block, prompting for the language.
With an active region, wrap the region in the block; otherwise
leave point on an empty line inside the block."
  (interactive)
  (let ((lang (string-trim (read-string "language: " nil 'org/code-block-history)))
        (region? (use-region-p)))
    (unless (string-empty-p lang)
      (add-to-history 'org/code-block-history lang))
    (org-insert-structure-template (string-trim (concat "src " lang)))
    (unless (or region? (string-empty-p lang))
      (open-line 1))))

(defun org/add-inline-code ()
  "Insert inline code wrapped in ~ markers.
With an active region, wrap the region; otherwise read the code
from the minibuffer."
  (interactive)
  (if (use-region-p)
      (org-emphasize ?~)
    (let ((code (read-string "code: " nil 'org/inline-code-history)))
      (add-to-history 'org/inline-code-history code)
      (insert (format "~%s~" code)))))

(defun org-mode-tools ()
  "Bind org editing helpers in the current buffer."
  (local-set-key (kbd "C-c b") 'org/add-code-block)
  (local-set-key (kbd "C-c m") 'org/add-inline-code))

(add-hook 'org-mode-hook 'org-mode-tools)

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

(add-hook 'haskell-mode-hook 'haskell-config-setup-haskell-mode)

(defun haskell-config-setup-cabal-mode()
  "Setup the cabal editing environment."
  (local-set-key (kbd "C-)") 'forward-sexp)
  (local-set-key (kbd "C-(") 'backward-sexp)
  (local-set-key (kbd "C-<tab>") 'cabal-pretty-print-buffer)
  )

(add-hook 'haskell-cabal-mode-hook 'haskell-config-setup-cabal-mode)
(add-hook 'before-save-hook 'haskell-config-save-hook)

;; -------------------------------------------------------------------
;; Cheatsheets:  M-x cheatsheet  /  :cheatsheet [NAME]
;;
;; Each sheet is a plain-text file, ~/.emacs.d/cheatsheets/NAME.txt,
;; installed from emacs/emacs.d/cheatsheets in home-manager.  With no
;; argument the sheets for the current buffer are shown: any active
;; minor mode with a sheet (e.g. pml-mode), then the major mode and its
;; parents (gfm-mode falls back to markdown-mode).  `:cheatsheet gptel'
;; or `C-u M-x cheatsheet' picks a sheet by name.
;;
;; Sheet format: a line indented by exactly two spaces is a key entry,
;; "  KEY  description", where KEY is in `kbd' syntax.  Each KEY is
;; looked up in the buffer the command was run from, and entries that
;; are not bound there are flagged, so a sheet can't silently drift
;; from the real bindings.  Everything else is free text.
;; -------------------------------------------------------------------
(defvar cheatsheet-directory (expand-file-name "cheatsheets" user-emacs-directory)
  "Directory holding cheatsheet text files, one NAME.txt per sheet.")

(defconst cheatsheet--key-line-regexp "^  \\([^ ].*?\\)  +\\S-"
  "Match a \"  KEY  description\" line; group 1 is KEY.")

(defun cheatsheet--file (name)
  "Path of the cheatsheet called NAME."
  (expand-file-name (concat name ".txt") cheatsheet-directory))

(defun cheatsheet--available ()
  "Names of every sheet in `cheatsheet-directory'."
  (when (file-directory-p cheatsheet-directory)
    (mapcar #'file-name-sans-extension
            (directory-files cheatsheet-directory nil "\\.txt\\'"))))

(defun cheatsheet--names-for-buffer ()
  "Sheet names that apply to the current buffer.
Active minor modes come first, then the major mode and its parents."
  (let ((modes (seq-filter (lambda (m) (and (boundp m) (symbol-value m)))
                           minor-mode-list))
        (mode major-mode))
    (while mode
      (setq modes (append modes (list mode)))
      (setq mode (get mode 'derived-mode-parent)))
    (seq-filter (lambda (name) (file-readable-p (cheatsheet--file name)))
                (mapcar #'symbol-name modes))))

(defun cheatsheet--key-bound-p (key)
  "Non-nil if KEY, a `kbd' string, is bound in the current buffer."
  (condition-case nil
      (let ((binding (key-binding (kbd key))))
        (and binding (not (numberp binding))))
    (error nil)))

(defun cheatsheet--render (name source-buffer)
  "Return the text of sheet NAME, flagging keys unbound in SOURCE-BUFFER."
  (with-temp-buffer
    (insert-file-contents (cheatsheet--file name))
    (goto-char (point-min))
    (while (re-search-forward cheatsheet--key-line-regexp nil t)
      (let ((key (match-string 1)))
        (unless (with-current-buffer source-buffer (cheatsheet--key-bound-p key))
          (end-of-line)
          (insert "   [not bound in this buffer]"))))
    (buffer-string)))

(define-derived-mode cheatsheet-mode special-mode "Cheatsheet"
  "Read-only display of a cheatsheet.  Press q to close it.")

(with-eval-after-load 'evil
  (evil-set-initial-state 'cheatsheet-mode 'motion))

(defun cheatsheet (&optional name)
  "Show the cheatsheets for the current buffer, or the sheet called NAME.
With a prefix argument, prompt for NAME."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Cheatsheet: " (cheatsheet--available) nil t))))
  (let* ((source (current-buffer))
         (available (cheatsheet--available))
         (names (cond ((and name (not (string-empty-p name))) (list name))
                      ((cheatsheet--names-for-buffer))
                      (t (list (completing-read
                                (format "No cheatsheet for %s; show: " major-mode)
                                available nil t)))))
         (missing (seq-remove (lambda (n) (member n available)) names)))
    (when missing
      (user-error "No cheatsheet named %s (available: %s)"
                  (car missing) (string-join available ", ")))
    (let ((buf (get-buffer-create "*cheatsheet*"))
          (others (seq-difference available names)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (string-join
                   (mapcar (lambda (n) (cheatsheet--render n source)) names)
                   "\n\n"))
          (when others
            (insert (format "\n\nOther sheets: %s   (:cheatsheet NAME)\n"
                            (string-join others ", "))))
          (goto-char (point-min)))
        (cheatsheet-mode))
      (select-window
       (display-buffer buf '(display-buffer-at-bottom
                             . ((window-height . fit-window-to-buffer))))))))

;; evil is required unconditionally near the top of this file.
(evil-define-command cheatsheet-ex (&optional name)
  "Show a cheatsheet from the ex command line: `:cheatsheet [NAME]'."
  (interactive "<a>")
  (cheatsheet name))
(evil-ex-define-cmd "cheatsheet" 'cheatsheet-ex)

;;; init.el ends here
