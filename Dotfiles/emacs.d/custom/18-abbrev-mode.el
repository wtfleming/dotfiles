;; abbrev-mode config

;; To add abbreviations type the word you want to use as expansion, and then
;; type 'C-x a g' and the abbreviation for it.

(setq abbrev-file-name             ;; tell emacs where to read abbrev
  "~/.emacs.d/custom/abbrev-defs") ;; definitions from

(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                   ;; you will be asked before the abbreviations are saved

(setq-default abbrev-mode t)       ;; Turn on abbrev mode globally


;; If you only want it on in text and derived modes, you could do something like this:
;;(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

;; For multiple modes, use something like the following:
;;(dolist (hook '(erc-mode-hook
;;emacs-lisp-mode-hook
;;text-mode-hook))
;;(add-hook hook (lambda () (abbrev-mode 1))))
