;; Turn off mouse interface early in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(add-to-list 'load-path "~/.emacs.d/custom")
(load "00-functions.el")
(load "01-common-setup.el")
(load "02-magit.el")
(load "03-org-mode.el")
(load "04-paren-mode.el")
(load "05-uniquify.el")
(load "06-expand-region.el")
(load "07-scala-mode-2.el")
(load "08-clojure.el")
(load "09-dired.el")
(load "10-javascript-mode.el")
(load "11-saveplace.el")
(load "12-recentf.el")
(load "13-ispell.el")
(load "14-tramp.el")
(load "15-ibuffer.el")
(load "16-helm.el")
(load "17-yasnippet.el")
(load "18-abbrev-mode.el")
(load "19-csharp-mode.el")
