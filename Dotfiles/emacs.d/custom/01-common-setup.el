(setq package-user-dir (format "~/.emacs.d/elpa-%d" emacs-major-version))

(require 'package)
(setq package-enable-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Requires Emacs 24.4 or newer
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Check if the packages are installed; if not, install them.
;; The very first time you start Emacs you will need to run:
;; M-x package-list-packages
;; and then restart Emacs.

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(all-the-icons beacon color-identifiers-mode csharp-mode doom-modeline elixir-mode exunit flycheck-inline git-gutter helm helm-lsp helm-projectile hydra js2-mode lsp-mode lsp-ui lua-mode multiple-cursors neotree omnisharp projectile rainbow-delimiters rainbow-mode scala-mode shader-mode tern tide yasnippet lsp-metals))


;; Note that for all-the-icons to work you must manually install them by calling
;; M-x all-the-icons-install-fonts

;; (use-package yasnippet
;;   :ensure t
;;   :commands yas-minor-mode
;;   :hook (go-mode . yas-minor-mode))

;; Dired
(require 'dired )
(setq dired-listing-switches "-lh")


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-lsp
  :ensure t
  :commands company-lsp)


(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))


;; Zenburn
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; uniquify
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; https://www.emacswiki.org/emacs/MidnightMode
;; By default, the ‘midnight-hook’ is configured to just run the CleanBufferList command.
(use-package midnight
  :init
  (setq clean-buffer-list-kill-never-buffer-names '("*httpd*"))
  :config
  (midnight-delay-set 'midnight-delay "4:30am"))


;; paren-mode
(setq show-paren-delay 0) ; how long to wait?
(show-paren-mode t) ; turn paren-mode on


;; neotree
;; https://github.com/jaypei/emacs-neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)


;; Beacon — Never lose your cursor again
;; https://github.com/Malabarba/beacon
(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#666600")


;; Maximize Emacs frame on startup
;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Enable projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(add-to-list 'projectile-globally-ignored-directories "node_modules")

;; Enable semantic-mode
(semantic-mode 1)

(setq visible-bell t)

;; Set default major mode to text-mode
(setq default-major-mode 'text-mode)

;; Enable flyspell in text-mode
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

;; Override opening the buffer menu so it happens in
;; the same window, rather than a new one.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

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
;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    backup-directory-alist
;;    '(("." . "~/.saves"))    ; don't litter my fs tree
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups

;; Disable backup files
(setq backup-inhibited t)
;; Disable auto save files
(setq auto-save-default nil)
;; Disable lock files - temp symlinks that start with .#
(setq create-lockfiles nil)

;; Ensure line and column numbers are displayed on the mode line
(setq line-number-mode t) ; Default is on for line, but set it anyways
(setq column-number-mode t)


;; Don't show the splash screen
(setq inhibit-startup-screen t)

;; Don't include a message in the *scratch* buffer
(setq initial-scratch-message "")

;; Set *scratch* buffer to use text-mode instead of lisp-interaction-mode
(setq initial-major-mode 'text-mode)

;; Don't bind (suspend-emacs)
(global-unset-key (kbd "C-z"))

;; Wind Move
; Move point from window to window using meta and the arrow keys,
; rather than having to use C-x o
(windmove-default-keybindings 'meta)

;; ------------------------------------------------

;; Highlight current line of characters
(global-hl-line-mode t)

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

;; Ask before exiting emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; Enable uppercasing and lowercasing on regions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Save Place
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(save-place-mode 1)

;; recentf
(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/.recentf"
        recentf-max-saved-items 500
        recentf-max-menu-items 25
        ;; disable recentf-cleanup on Emacs start, because it can cause problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; tramp
(setq tramp-default-method "ssh")

