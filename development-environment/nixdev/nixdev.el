(defun my-nix-mode-config ()
  "Configure defaults for editing nix expressions."
  (default-programming-config)
  )

(add-hook 'nix-mode 'my-nix-mode-config)
