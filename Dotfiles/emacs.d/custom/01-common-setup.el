(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Requires Emacs 24.4 or newer
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode-extra-font-locking . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

(package-initialize)

;; Check if the packages are installed; if not, install them.
;; The very first time you start Emacs you will need to run:
;; M-x package-list-packages
;; and then restart Emacs.

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(zenburn-theme rainbow-mode expand-region cider clojure-mode clojure-mode-extra-font-locking color-identifiers-mode magit git-gutter company helm projectile helm-projectile yasnippet paredit rainbow-delimiters beacon elixir-mode alchemist csharp-mode omnisharp lua-mode))

;; Load Zenburn
(load-theme 'zenburn t)

;; Beacon — Never lose your cursor again
;; https://github.com/Malabarba/beacon
(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#666600")

;; Enable paredit in some modes
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Enable rainbow-delimiters-mode when programming
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Maximize Emacs frame on startup
;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable git-gutter globally
(global-git-gutter-mode +1)

;; Enable projectile
(projectile-global-mode)

;; Enable semantic-mode
(semantic-mode 1)

;; Enable company mode for auto-completion
(add-hook 'after-init-hook 'global-company-mode)

;; Disable emacs built in version control for faster startup
;; Use magit instead of it
(setq vc-handled-backends ())

;; No beep warning or flash
(setq ring-bell-function 'ignore)

;; Set default major mode to text-mode
(setq default-major-mode 'text-mode)

;; Enable flyspell in text-mode
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

;; Override opening the buffer menu so it happens in
;; the same window, rather than a new one.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Use command as meta on OS X
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; Let M-x toggle-frame-fullscreen work correctly on OS X
(setq ns-use-native-fullscreen nil)

;; Map C-x C-u to undo
(define-key global-map "\C-x\C-u" 'undo)

;; Set font size to 12pt
(set-face-attribute 'default nil :height 120)

;; Open .gz, etc files for editing
(auto-compression-mode 1)

;; ----------- emacs shell ----------------------------
;; Emacs shell
; Dont echo passwords
(add-hook 'comint-output-filter-functions
      'comint-watch-for-password-prompt)

;; Clear shell buffer with C-c l (like C-l in a terminal)
(defun my-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun my-shell-hook ()
  (local-set-key "\C-cl" 'my-clear))

(add-hook 'shell-mode-hook 'my-shell-hook)
;; -------------------------------------------

;; Use y or n for emacs yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; Set *scratch* buffer to use text-mode instead of lisp-interaction-mode
(setq initial-major-mode 'text-mode)


;; don't let me accidentally exit out of emacs
;; use "M-x kill-emacs" instead
;;(global-unset-key "\C-x\C-c")

;; Wind Move
; Move point from window to window using meta and the arrow keys,
; rather than having to use C-x o
(windmove-default-keybindings 'meta)

;; Enable trailing whitespace in programming modes
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (set-variable 'show-trailing-whitespace t))))
;; ------------------------------------------------

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
