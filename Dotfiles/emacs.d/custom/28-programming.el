;; Enable rainbow-delimiters-mode when programming
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Enable git-gutter globally
(global-git-gutter-mode +1)

;; Enable trailing whitespace in programming modes
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (set-variable 'show-trailing-whitespace t))))

;; Disable emacs built in version control for faster startup
(setq vc-handled-backends ())

(add-hook 'go-mode-hook (lambda () (subword-mode +1)))
(add-hook 'elixir-mode-hook (lambda () (subword-mode +1)))
(add-hook 'rust-mode-hook (lambda () (subword-mode +1)))

;; Use shader-mode for Unity shaders
(add-to-list 'auto-mode-alist '("\\.shader\\'" . shader-mode))
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . shader-mode))

;; ------- magit -------
(use-package magit
             :ensure t
             :bind (("C-c m" . magit-status)))


;; ------- Rust -------
;; Run this command in a terminal to install the rust language server
;; rustup component add rls rust-analysis rust-src

(use-package rust-mode
  :ensure t
  :mode ("\\.rust$" . rust-mode)
  :commands (rust-mode)
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))

  (setq rust-format-on-save t)
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  (add-hook 'rust-mode-hook (lambda () (flycheck-mode +1))))

(use-package cargo
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :if (featurep 'flycheck)
  :after (rust-mode flycheck)
  :init
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

;; ------- TypeScript -------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ; (flycheck-select-checker 'typescript-tslint)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Move cursor by camelCase
(add-hook 'typescript-mode-hook (lambda () (subword-mode +1)))

(setq typescript-indent-level 2)

;; ------- JavaScript -------
;; Javascript mode

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
; (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))


(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (subword-mode +1)))
;;(add-hook 'js2-mode-hook (lambda () (flycheck-mode t)))


;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-tern))

;; (add-hook 'js2-mode-hook
;;           (defun my-js2-mode-setup ()
;;             (flycheck-mode t)
;;             (when (executable-find "eslint")
;;               (flycheck-select-checker 'javascript-eslint))))

;; ------- C# -------
(defun my-csharp-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "k&r")
  (setq c-basic-offset 2)
  (setq truncate-lines t)
  (setq tab-width 2)
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

;; csharp-mode README.md recommends this too
;; (electric-pair-mode 1)       ;; Emacs 24
;; (electric-pair-local-mode 1) ;; Emacs 25

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(add-hook 'csharp-mode-hook 'flycheck-mode)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; Move cursor by camelCase
(add-hook 'csharp-mode-hook (lambda () (subword-mode +1)))


(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook #'company-mode)

;; ------- Go -------
(use-package go-mode
  :defer t
  :ensure t
  :mode ("\\.go\\'" . go-mode))

;; ------- web-mode -------
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jst.ejs\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.jsx$" . web-mode)
  ("\\.tsx$" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.leex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t))


;; ------- Misc -------
(use-package dockerfile-mode
  :ensure t
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;; https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :ensure t)


;; Compatability with HCL and Terraform syntax
(use-package hcl-mode
  :ensure t
  :mode (("\\.tpl\\'" . hcl-mode)
         ("\\.tf\\'" . hcl-mode)))
