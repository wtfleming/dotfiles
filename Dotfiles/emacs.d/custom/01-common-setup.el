(require 'package)
(add-to-list 'package-archives
  	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Check if the packages are installed; if not, install them.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(zenburn-theme scala-mode2 rainbow-mode expand-region cider color-identifiers-mode magit pig-mode))

(load-theme 'zenburn t)

;; Disable emacs built in version control for faster startup
;; Use magit instead of it
(setq vc-handled-backends ())

;; Override opening the buffer menu so it happens in
;; the same window, rather than a new one.
(global-set-key (kbd "C-x C-b") 'buffer-menu)


;; Use command as meta on OS X
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; Map C-x C-u to undo
(define-key global-map "\C-x\C-u" 'undo)

;; Set font size to 12pt
(set-face-attribute 'default nil :height 120)

;; Open .gz, etc files for editing
(auto-compression-mode 1)

;; Emacs shell
; Dont echo passwords
(add-hook 'comint-output-filter-functions
      'comint-watch-for-password-prompt)


;; Backup files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;; Ensure line and column numbers are displayed on the mode line
(setq line-number-mode t) ; Default is on for line, but set it anyways
(setq column-number-mode t)


;; Don't show the splash screen
(setq inhibit-startup-screen t)

;; Don't include a message in the *scratch* buffer
(setq initial-scratch-message "")


;; don't let me accidentally exit out of emacs
;; use "M-x kill-emacs" instead
;;(global-unset-key "\C-x\C-c")

;; Wind Move
; Move point from window to window using meta and the arrow keys,
; rather than having to use C-x o
(windmove-default-keybindings 'meta)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Highlight current line of characters
(global-hl-line-mode t)

;; Open emacs in front of the terminal window on OS X instead of behind
;; http://stackoverflow.com/questions/10171280/how-to-launch-gui-emacs-from-command-line-in-osx
(if (system-type-is-darwin)
  (x-focus-frame nil))

(setq default-directory "~/")


;; Automatically indent new lines in programming major modes
;; For now turn off since it does not seem to play well with org-mode
;(electric-indent-mode +1)

;; Use Emacs terminfo, not system terminfo
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setq system-uses-terminfo nil)

;; Prefer utf-8 encoding
(prefer-coding-system 'utf-8)

;; Pasting over something kills it
(delete-selection-mode 1)

;; No tabs in indentation
(setq-default indent-tabs-mode nil)
