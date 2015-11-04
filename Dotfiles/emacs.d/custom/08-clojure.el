;;; cider
;; Enable http://emacswiki.org/emacs/ElDoc
(add-hook 'cider-mode-hook #'eldoc-mode)

;; Hide the *nrepl-connection* and *nrepl-server* buffers from appearing in some buffer switching commands like switch-to-buffer(C-x b)
;(setq nrepl-hide-special-buffers t)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; Enable http://wikemacs.org/wiki/Subword-mode
(add-hook 'cider-repl-mode-hook #'subword-mode)

(add-hook 'cider-repl-mode-hook #'paredit-mode)


;;; clojure-mode
;; Ensure we only indent 2 spaces for clojure-mode
;(setq lisp-indent-offset 2)
;(setq clojure-defun-style-default-indent t)

(eval-after-load 'clojure-mode '(require 'clojure-mode-extra-font-locking))

;; Enable http://wikemacs.org/wiki/Subword-mode
(add-hook 'clojure-mode-hook #'subword-mode)

(add-hook 'clojure-mode-hook #'paredit-mode)
