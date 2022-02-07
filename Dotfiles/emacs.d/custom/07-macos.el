;; Open emacs in front of the terminal window on OS X instead of behind
;; http://stackoverflow.com/questions/10171280/how-to-launch-gui-emacs-from-command-line-in-osx
(if (system-type-is-darwin)
  (x-focus-frame nil))

;; Use command as meta on OS X
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; Let M-x toggle-frame-fullscreen work correctly on OS X
(setq ns-use-native-fullscreen nil)

;; ispell installed with homebrew
;; TODO detect where homebrew installed ispell, it could be either of these two
;; (if (system-type-is-darwin)
;;   (setq ispell-program-name "/usr/local/bin/ispell"))

(if (system-type-is-darwin)
  (setq ispell-program-name "/opt/homebrew/bin/ispell"))
