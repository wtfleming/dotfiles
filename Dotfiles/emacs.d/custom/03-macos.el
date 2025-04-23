;; Open emacs in front of the terminal window on OS X instead of behind
;; http://stackoverflow.com/questions/10171280/how-to-launch-gui-emacs-from-command-line-in-osx
(if (system-type-is-darwin)
  (x-focus-frame nil))

;; Use command as meta on OS X
(setopt mac-option-modifier 'super)
(setopt mac-command-modifier 'meta)

;; Let M-x toggle-frame-fullscreen work correctly on OS X
(setopt ns-use-native-fullscreen nil)

(if (system-type-is-darwin)
  (setopt ispell-program-name "/opt/homebrew/bin/ispell"))
