(defun my-haskell-mode-hooks()

  "Create some custom haskell-mode hooks."
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

  (rainbow-delimiters-mode)
  (local-set-key (kbd "C-)") 'forward-sexp)
  (local-set-key (kbd "C-(") 'backward-sexp)
  (local-set-key (kbd "C-<tab>") 'hsfmt)
  (turn-on-line-numbers)
  (set-face-attribute 'default nil
                      :family "Hasklig"
                      :weight 'normal
                      :width 'normal)
  (hasklig-mode)
  )

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
  (local-set-key (kbd "C-<tab>") 'hsfmt)
  )

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hooks)
(add-hook 'haskell-cabal-mode-hook 'my-haskell-cabal-mode)
