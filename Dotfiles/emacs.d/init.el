;; Write any customized variables to a specific file instead of this file
(setopt custom-file "~/.emacs.d/my-customized.el")
(load custom-file)

;; Turn off mouse interface early in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Load files from "~/.emacs.d/custom"
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
(load "00-functions.el")
(load "01-common-setup.el")
(load "02-org-mode.el")
(load "03-macos.el")
(load "04-ibuffer.el")
(load "05-helm.el")
(load "06-hydra.el")
(load "07-doom-modeline.el")
(load "08-programming.el")
(load "09-gptel.el")
(load "10-transient.el")

(when (file-exists-p "~/.emacs.d/lisp/rama-mode.el")
  (load "~/.emacs.d/lisp/rama-mode.el"))
