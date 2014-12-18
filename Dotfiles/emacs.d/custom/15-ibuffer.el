(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("erc" (mode . erc-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*Messages\\*$")))
               ("org" (mode . org-mode))
               ("Programming"
                (or
                 (mode . pig-mode)
                 (mode . scala-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 ;; etc
                 ))
               ))))


(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
