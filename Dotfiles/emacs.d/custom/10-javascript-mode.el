;; Javascript mode

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ; Use tabs instead of spaces just for javascript mode
(add-hook 'js2-mode-hook (lambda ()
                          (setq c-basic-offset 4
                                tab-width 4
                                indent-tabs-mode t)))


(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))


;; Old js-mode config:

;; (setq js-indent-level 2)

;; (add-hook 'js-mode-hook 'js2-minor-mode)

;; ; Use tabs instead of spaces just for javascript mode
;; (add-hook 'js-mode-hook (lambda ()
;;                           (setq c-basic-offset 4
;;                                 tab-width 4
;;                                 indent-tabs-mode t)))
