;; ;;; emacs-haskell-config.el --- A customized haskell editing config

;; ;;; Commentary:
;; ;; This package provides a custom haskell environment setup in the form of a
;; ;; package that can be easily added to any other Emacs config.


;; ;;; Code:

;; (use-package nix-haskell-mode
;;   :hook (haskell-mode . nix-haskell-mode))

;; (defcustom haskell-pretty-printer nil
;;   "Program used to reformat haskell source code."
;;   :group 'haskell-config
;;   :type '(choice (const "stylish-haskell")
;;                  (const "fourmolu")
;;                  (const "ormolu")
;;                  (const "brittany")
;;                  (const nil)
;;                  (string :tag "other formatter")))

;; (defcustom haskell-format-on-save nil
;;   "If enabled, format haskell buffer on save."
;;   :group 'haskell-config
;;   :type '(boolean))

;; (defcustom cabal-pretty-printer nil
;;   "Program used to reformat cabal configurations."
;;   :group 'haskell-config
;;   :type '(choice (const "stylish-cabal")
;;                  (const nil)
;;                  (string :tag "other formatter")))

;; (defcustom cabal-format-on-save nil
;;   "If enabled, format cabal buffer on safe."
;;   :group 'haskell-config
;;   :type '(boolean))

;; ;; HLS is still a little bit buggy on some codebases, so make it configurable
;; ;; whether you want to default to HLS when opening files, or enable it
;; ;; explicitly.
;; (defcustom haskell-default-to-hls nil
;;   "Default to using haskell-language-server with LSP mode instead of haskell-mode."
;;   :group 'haskell-config
;;   :type '(boolean))

;; (defun pretty-print-buffer(format-command)
;;   "Run FORMAT-COMMAND to pretty-print the current buffer."
;;   ((defvar-local p (point))
;;     (shell-command-on-region (point-min) (point-max) format-command nil t)
;;     (goto-char p)
;;     )
;;   )

;; (defun haskell-pretty-print-buffer()
;;   "Pretty-print a haskell buffer using haskell-pretty-printer."
;;   (interactive)
;;   (when haskell-pretty-printer (pretty-print-buffer haskell-pretty-printer))
;;   )

;; (defun cabal-pretty-print-buffer()
;;   "Pretty-print a cabal buffer using cabal-pretty-printer."
;;   (interactive)
;;   ((when cabal-pretty-printer (pretty-print-buffer cabal-pretty-printer)))
;;   )

;; (defun haskell-config-save-hook()
;;   "Save hook function will automatically format a haskell or cabal buffer on save."
;;   (if
;;       (and (eq major-mode 'haskell-mode) (haskell-format-on-save))
;;       (haskell-pretty-print-buffer)
;;     (if
;;         (and (eq major-mode 'haskell-cabal-mode) (cabal-format-on-save))
;;         (cabal-pretty-print-buffer))))


;; (defun haskell-config-setup-haskell-mode()
;;   "Setup the haskell editing environment."

;;   (setq haskell-tags-on-save t)
;;   (rainbow-delimiters-mode t)
;;   (turn-on-line-numbers)

;;   (set-face-attribute 'default nil
;;                       :family "Hasklig"
;;                       :weight 'normal
;;                       :width 'normal)
;;   (hasklig-mode)


;;   (local-set-key (kbd "C-)") 'forward-sexp)
;;   (local-set-key (kbd "C-(") 'backward-sexp)
;;   (local-set-key (kbd "C-<tab>") 'haskell-pretty-print-buffer)
;;   (local-set-key (kbd "M-.") 'haskell-mode-tag-find)

;;   (custom-set-variables
;;     '(haskell-process-suggest-remove-import-lines t)
;;     '(haskell-process-auto-import-loaded-modules t)
;;     '(haskell-process-log t))

;;   (eval-after-load 'haskell-mode '(progn
;;     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;;     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))

;;   (eval-after-load 'haskell-cabal '(progn
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;;   )
;; (add-hook 'haskell-mode-hook 'haskell-config-setup-haskell-mode)

;; (defun haskell-config-setup-cabal-mode()
;;   "Setup the cabal editing environment."
;;   (local-set-key (kbd "C-)") 'forward-sexp)
;;   (local-set-key (kbd "C-(") 'backward-sexp)
;;   (local-set-key (kbd "C-<tab>") 'cabal-pretty-print-buffer)
;;   )
;; (add-hook 'haskell-cabal-mode-hook 'haskell-config-setup-cabal-mode)

;; (add-hook 'before-save-hook 'haskell-config-save-hook)
