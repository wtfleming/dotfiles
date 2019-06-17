(defun my-csharp-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "k&r")
  (setq c-basic-offset 2)
  (setq truncate-lines t)
  (setq tab-width 2)
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

;csharp-mode README.md recommends this too
;(electric-pair-mode 1)       ;; Emacs 24
;(electric-pair-local-mode 1) ;; Emacs 25

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(add-hook 'csharp-mode-hook 'flycheck-mode)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
