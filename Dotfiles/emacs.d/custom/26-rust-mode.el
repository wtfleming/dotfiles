(require 'rust-mode)

(add-hook 'rust-mode-hook 'cargo-minor-mode)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; (setq rust-format-on-save t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook (lambda () (flycheck-mode +1)))
