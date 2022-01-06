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
