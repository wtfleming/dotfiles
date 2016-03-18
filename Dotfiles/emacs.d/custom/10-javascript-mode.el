;; Javascript mode
;(setq js-indent-level 2)

(add-hook 'js-mode-hook 'js2-minor-mode)

; Use tabs instead of spaces just for javascript mode
(add-hook 'js-mode-hook (lambda ()
                          (setq c-basic-offset 4
                                tab-width 4
                                indent-tabs-mode t)))
