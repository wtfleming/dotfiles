;; ispell macports
;(if (system-type-is-darwin)
;  (setq ispell-program-name "/opt/local/bin/ispell"))


;; ispell homebrew
(if (system-type-is-darwin)
  (setq ispell-program-name "/usr/local/bin/ispell"))
