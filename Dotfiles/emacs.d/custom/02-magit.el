;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;; On OS X make magit commits work with emacs installed via homebrew
(if (system-type-is-darwin)
  (set-variable 'magit-emacsclient-executable "/usr/local/Cellar/emacs/24.4/bin/emacsclient"))

;; On OS X make magit commits work with emacs installed via macports
;(if (system-type-is-darwin)
;  (set-variable 'magit-emacsclient-executable "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient"))
