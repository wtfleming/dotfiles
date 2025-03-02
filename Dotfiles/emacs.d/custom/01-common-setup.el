;; Store downloaded packages in a directory corresponding to the emacs version we are running
;; Make upgrading emacs to a new version easier/safer
(setq package-user-dir (format "~/.emacs.d/elpa-%d" emacs-major-version))

;; Fix problem where emacs can not connect to melpa
;; https://emacs.stackexchange.com/questions/51721/failed-to-download-gnu-archive
;; TODO 2/15/25 - is this still a problem?
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;; ---- Emacs garbage collection ----
;; Show how long we are spending doing GC
;; see https://akrl.sdf.org/#orgc15a10d
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Set garbage collection threshold
;; (setq gc-cons-threshold #x40000000) ;; 1GB
(setq gc-cons-threshold (* 800 1024 1024)) ;; 800mb

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

;; ---- package management ----

(require 'package)
(setq package-enable-startup nil)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(gptel . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(helm . "melpa-stable") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Helm appears to be using ffap internally which can cause
;; network requests if you M-x while the cursor is on a URL
;; causing emacs to lock up for up to a few seconds, disable this.
;; See https://github.com/emacs-helm/helm/issues/648
(setq ffap-machine-p-known 'reject)

;; Note that for all-the-icons to work you must manually install them by calling
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)


;; Fonts
(defun font-available-p (font-name)
  (find-font (font-spec :name font-name)))

;; (cond
;;  ((font-available-p "Cascadia Code")
;;   (set-frame-font "Cascadia Code-12"))
;;  ((font-available-p "Menlo")
;;   (set-frame-font "Menlo-12"))
;;  ((font-available-p "DejaVu Sans Mono")
;;   (set-frame-font "DejaVu Sans Mono-12"))
;;  ((font-available-p "Inconsolata")
;;   (set-frame-font "Inconsolata-12")))

;; ------- Keybindings -------

;; Override opening the buffer menu so it happens in the same window, rather than a new one.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Map C-x C-u to undo
(define-key global-map "\C-x\C-u" 'undo)

;; Don't bind (suspend-emacs)
(global-unset-key (kbd "C-z"))

;; Wind Move
;; Move point from window to window using meta and the arrow keys,
;; rather than having to use C-x o
(windmove-default-keybindings 'meta)


;; ------- Visual Settings -------

;; Use a larger font on bigger monitors
(if (> (display-pixel-width) 1440)
  (set-face-attribute 'default nil :height 200)
  (set-face-attribute 'default nil :height 120))

;; Ensure line and column numbers are displayed on the mode line
(setq line-number-mode t) ; Default is on for line, but set it anyways
(setq column-number-mode t)

(setq visible-bell t)

;; Maximize Emacs frame on startup
;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Alterntively, you can set the dimensions of the initial frame like this
;;(setq initial-frame-alist '((top . 0) (left . 0) (width . 120) (height . 80)))

;; Highlight current line of characters
(global-hl-line-mode t)


;; ------- Misc -------

;; Don't show the splash screen
(setq inhibit-startup-screen t)

;; Don't include a message in the *scratch* buffer
(setq initial-scratch-message "")

(setq default-directory "~/")

;; Enable semantic-mode
;; TODO do I still want this enabled now that I mostly use lsp-mode?
(semantic-mode 1)

;; Set default major mode to text-mode
(setq default-major-mode 'text-mode)

;; Set *scratch* buffer to use text-mode instead of lisp-interaction-mode
(setq initial-major-mode 'text-mode)


;; Enable flyspell in text-mode
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

;; Use y or n for emacs yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Open .gz, etc files for editing
(auto-compression-mode 1)

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

;; ------- rainbow-mode -------
;; Colorize color names in buffers
;; For example: white or black or #000000
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))


;; ------- multiple-cursors -------
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))


;; ;; -------yasnippet -------
;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook 'yas-minor-mode)
;;   (add-hook 'text-mode-hook 'yas-minor-mode))

;; ;; The official collection of snippets for yasnippet.
;; ;; https://github.com/AndreaCrotti/yasnippet-snippets
;; (use-package yasnippet-snippets
;;   :ensure t)

;; ------- abbrev-mode -------
;; To add abbreviations type the word you want to use as expansion, and then
;; type 'C-x a g' and the abbreviation for it.

(setq abbrev-file-name             ;; tell emacs where to read abbrev
  "~/.emacs.d/custom/abbrev-defs") ;; definitions from

(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                   ;; you will be asked before the abbreviations are saved

(setq-default abbrev-mode t)       ;; Turn on abbrev mode globally


;; If you only want it on in text and derived modes, you could do something like this:
;;(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

;; For multiple modes, use something like the following:
;;(dolist (hook '(erc-mode-hook
;;emacs-lisp-mode-hook
;;text-mode-hook))
;;(add-hook hook (lambda () (abbrev-mode 1))))

;; ------- which-key -------
;; TODO this is built into emacs 30
;; when I get to that version can remove which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.05))

;; ------- Dired -------
(require 'dired )
(setq dired-listing-switches "-lh")

;; ------- Company -------
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;; ------- restclient -------
;; TODO this package is now archived https://github.com/pashky/restclient.el
;; look at alternatives like https://github.com/federicotdn/verb
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;; ------- zenburn-theme -------
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; ------- uniquify -------
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; ------- expand-region -------
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; ------- midnight -------
;; https://www.emacswiki.org/emacs/MidnightMode
;; By default, the ‘midnight-hook’ is configured to just run the CleanBufferList command.
(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "4:30am")
  (setq clean-buffer-list-delay-general 4))

;; ------- paren-mode -------
(setq show-paren-delay 0) ; how long to wait?
(show-paren-mode t) ; turn paren-mode on

;; ------- neotree -------
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :ensure t
  :init
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t))

;; ------- beacon -------
;; Beacon — Never lose your cursor again
;; https://github.com/Malabarba/beacon
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :config
  (setq beacon-push-mark 35)
  (setq beacon-color "#666600"))

;; ------- projectile -------
(use-package projectile
  :ensure t)

;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-mode +1)
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   :config
;;   (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; ------- ripgrep -------
;; install the binary with
;; brew install ripgrep
(use-package ripgrep
  :ensure t)

;; ------- org-reveal -------
;; https://github.com/hexmode/ox-reveal
;; Reveal.js is a tool for creating good-looking HTML presentations.
;; Org-Reveal exports your Org documents to reveal.js presentations.
(use-package ox-reveal
  :ensure t)

;; Can be used for syntax highlighting in org-reveal
(use-package htmlize
  :ensure t)

;; ----------- emacs shell ----------------------------
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

;; ------- Backup files -------
;; Disable backup files
(setq backup-inhibited t)
;; Disable auto save files
(setq auto-save-default nil)
;; Disable lock files - temp symlinks that start with .#
(setq create-lockfiles nil)

;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    backup-directory-alist
;;    '(("." . "~/.saves"))    ; don't litter my fs tree
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups



;; ------- Save Place -------
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(save-place-mode 1)

;; ------- recentf -------
(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/.recentf"
        recentf-max-saved-items 500
        recentf-max-menu-items 25
        ;; disable recentf-cleanup on Emacs start, because it can cause problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; ------- tramp -------
(setq tramp-default-method "ssh")

;; ------- gptel -------
;; Functions to include the gptel backend and model in responses from an LLM
(defun wtf-gptel-backend-and-model ()
  "Return gptel backend and model"
  (let ((backend (if (boundp 'gptel-backend) (aref gptel-backend 1)))
        (model (if (boundp 'gptel-model) gptel-model)))
    (format "(%s %s)" backend model)))

;; (defun wtf-gptel-insert-model-in-non-gptel-buffers ()
;;   "This function will add the backend and model in the \"dynamic\" buffers, not in dedicated chat buffers.
;; To be used in `gptel-pre-response-hook'."
;;   (unless (member 'gptel-mode local-minor-modes)
;;     (goto-char (point-max))
;;     (insert (format "\n%s: " (wtf-gptel-backend-and-model)))
;;     (goto-char (point-max))))

(defun wtf-gptel-insert-model-in-chat-buffers (response-begin-pos response-end-pos)
  "This function adds the backend and model in dedicated chat buffers.
Can be used with the `gptel-post-response-functions' hook."
  (let* ((gptel-org-prefix (alist-get 'org-mode gptel-prompt-prefix-alist))
         (inserted-string (format "%s %s\n"
                                  (substring gptel-org-prefix 0 (string-match " " gptel-org-prefix))
                                  (wtf-gptel-backend-and-model)))
         (len-inserted (length inserted-string )))
    (goto-char response-begin-pos)
    (insert inserted-string)
    (goto-char (+ response-end-pos len-inserted))))

;; For Ollama, You should have at least 8 GB of RAM available to run the 7B models,
;; 16 GB to run the 13B models, and 32 GB to run the 33B models.
(use-package gptel
  :ensure t
  :config
  ;; (add-hook 'gptel-pre-response-hook 'wtf-gptel-insert-model-in-non-gptel-buffers)
  (add-hook 'gptel-post-response-functions 'wtf-gptel-insert-model-in-chat-buffers)
  (gptel-make-ollama "Ollama" ; Can be any name of your choosing
    :host "localhost:11434"
    :stream t
    :models '(deepseek-r1:7b deepseek-r1:14b qwen2.5-coder:14b-instruct-q6_K gemma2 llava))
  (gptel-make-anthropic "Claude"
    :stream t
    :key gptel-api-key)) ; Fetches key from ~/.authinfo
