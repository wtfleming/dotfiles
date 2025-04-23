;; https://github.com/seagle0128/doom-modeline

;; Note that for all-the-icons to work you must manually install them by calling
;; M-x all-the-icons-install-fonts

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; (require 'doom-modeline)
;; (doom-modeline-mode 1)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setopt doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setopt doom-modeline-bar-width 3)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setopt doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line or not.
(setopt doom-modeline-icon t)

;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
(setopt doom-modeline-major-mode-icon t)

;; Whether display color icons for `major-mode'. It respects
;; `doom-modeline-icon' and `all-the-icons-color-icons'.
(setopt doom-modeline-major-mode-color-icon t)

;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
(setopt doom-modeline-buffer-state-icon t)

;; Whether display buffer modification icon. It respects `doom-modeline-icon'
;; and `doom-modeline-buffer-state-icon'.
(setopt doom-modeline-buffer-modification-icon t)

;; Whether display minor modes in mode-line or not.
(setopt doom-modeline-minor-modes nil)


;; If non-nil, a word count will be added to the selection-info modeline segment.
(setopt doom-modeline-enable-word-count nil)

;; Whether display buffer encoding.
(setopt doom-modeline-buffer-encoding t)

;; Whether display indentation information.
(setopt doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setopt doom-modeline-checker-simple-format t)

;; The maximum displayed length of the branch name of version control.
(setopt doom-modeline-vcs-max-length 12)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setopt doom-modeline-persp-name t)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setopt doom-modeline-lsp t)

;; Whether display github notifications or not. Requires `ghub` package.
(setopt doom-modeline-github nil)

;; The interval of checking github.
(setopt doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not
(setopt doom-modeline-env-version t)
;; Or for individual languages
(setopt doom-modeline-env-enable-python t)
(setopt doom-modeline-env-enable-ruby t)
(setopt doom-modeline-env-enable-perl t)
(setopt doom-modeline-env-enable-go t)
(setopt doom-modeline-env-enable-elixir t)
(setopt doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setopt doom-modeline-env-python-executable "python")
(setopt doom-modeline-env-ruby-executable "ruby")
(setopt doom-modeline-env-perl-executable "perl")
(setopt doom-modeline-env-go-executable "go")
(setopt doom-modeline-env-elixir-executable "iex")
(setopt doom-modeline-env-rust-executable "rustc")

;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
(setopt doom-modeline-mu4e t)

;; Whether display irc notifications or not. Requires `circe' package.
(setopt doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setopt doom-modeline-irc-stylize 'identity)
