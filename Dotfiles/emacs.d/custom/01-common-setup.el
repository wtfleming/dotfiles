;; Store downloaded packages in a directory corresponding to the emacs version we are running
;; Make upgrading emacs to a new major version easier/safer
(setopt package-user-dir (format "~/.emacs.d/elpa-%d" emacs-major-version))

;; Fix problem where emacs can not connect to melpa
;; https://emacs.stackexchange.com/questions/51721/failed-to-download-gnu-archive
;; TODO 2/15/25 - is this still a problem?
(setopt gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;; ---- Garbage collection ----
;;
;; Set garbage collection threshold
;; (setopt gc-cons-threshold #x40000000)     ;; 1GB
 (setopt gc-cons-threshold (* 800 1024 1024)) ;; 800mb
;;(setopt gc-cons-threshold (* 100 1024 1024)) ;; 100mb

;; Show how long we are spending doing GC
;; see https://akrl.sdf.org/#orgc15a10d
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 60 seconds run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 60 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

;; ---- package management ----
(require 'package)
;; If you want to see how long packages take to load
;; when emacs starts, uncomment the next line
;; (setopt use-package-compute-statistics t)
;; then evaluate this function
;; (use-package-report)
;;
;; Output will look like this, and helps identify slow loading
;; packages that could potentially have their loading deferred
;; ox-reveal                 Configured    11:43:16.207041         0.77
;; treesit                   Configured    11:43:16.604111         0.34
;; js2-mode                  Configured    11:43:16.790564         0.29


(setopt package-enable-startup nil)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(gptel . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(helm . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(helm-core . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(lsp-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(lsp-ui . "melpa-stable") t)
;; is this needed now that transient is built in to emacs?
(add-to-list 'package-pinned-packages '(transient . "melpa-stable") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package-hook-name-suffix is a variable defined in ‘use-package-core.el’.
;; Its default value is "-hook"
;; Text append to the name of hooks mentioned by :hook.
;; Set to nil if you don’t want this to happen; it’s only a convenience.
;;
;; Or to put another way the way use-package works by default is to add
;; the value of this variable to the name of the hook.
;; So the default way to add a hook would look like:
;; :hook (after-init . projectile-mode)
;; but with use-package-hook-name-suffix set to nil it would be:
;; :hook (after-init-hook . projectile-mode)
;; (setopt use-package-hook-name-suffix nil)


;; Helm appears to be using ffap internally which can cause
;; network requests if you M-x while the cursor is on a URL
;; causing emacs to lock up for up to a few seconds, disable this.
;; See https://github.com/emacs-helm/helm/issues/648
(setopt ffap-machine-p-known 'reject)

;; Note that for all-the-icons to work you must manually install them by calling
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))


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
(setopt line-number-mode t) ; Default is on for line, but set it anyways
(setopt column-number-mode t)

(setopt visible-bell t)

;; Maximize Emacs frame on startup
;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Alternatively, you can set the dimensions of the initial frame like this
;;(setopt initial-frame-alist '((top . 0) (left . 0) (width . 120) (height . 80)))

;; Highlight current line of characters
(global-hl-line-mode t)

;; ------- flyspell -------
;; Enable flyspell in text-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; When programming, enable Flyspell mode for comments and strings only.
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; ------- Misc -------

;; Don't show the splash screen
(setopt inhibit-startup-screen t)

;; Don't include a message in the *scratch* buffer
(setopt initial-scratch-message "")

(setopt default-directory "~/")

;; Enable semantic-mode
;; TODO do I still want this enabled now that I mostly use lsp-mode?
(semantic-mode 1)

;; Set default major mode to text-mode
(setopt default-major-mode 'text-mode)



;; Use y or n for emacs yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Open .gz, etc files for editing
(auto-compression-mode 1)

;; Use Emacs terminfo, not system terminfo
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal
(setopt system-uses-terminfo nil)

;; Prefer utf-8 encoding
(prefer-coding-system 'utf-8)

;; Pasting over something kills it
(delete-selection-mode 1)

;; No tabs in indentation
(setq-default indent-tabs-mode nil)

;; Ask before exiting emacs
(setopt confirm-kill-emacs #'y-or-n-p)

;; Enable uppercasing and lowercasing on regions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; ------- rainbow-mode -------
;; Colorize color names in programming buffers
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

(setopt abbrev-file-name           ;; tell emacs where to read abbrev
  "~/.emacs.d/custom/abbrev-defs") ;; definitions from

(setopt save-abbrevs t)            ;; save abbrevs when files are saved
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
  :custom
  (which-key-sort-order 'which-key-description-order)
  (which-key-side-window-max-width 0.33)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.05))

;; By default, Which-key doesn't give much help for prefix-keys.  It
;; either shows the generic description, "+prefix", or the name of a
;; prefix-command, which usually isn't as descriptive as we'd like.
;;
;; Here are some descriptions for the default bindings in `global-map'
;; and `org-mode-map'.
(which-key-add-key-based-replacements
  "<f1> 4"        "help-other-win"
  "<f1>"          "help"
  "<f2>"          "2-column"
  "C-c"           "mode-and-user"
  "C-h 4"         "help-other-win"
  "C-h"           "help"
  "C-x 4"         "other-window"
  "C-x 5"         "other-frame"
  "C-x 6"         "2-column"
  "C-x 8 e"       "insert-emoji"
  "C-x 8"         "insert-special"
  "C-x C-k C-q"   "kmacro-counters"
  "C-x C-k C-r a" "kmacro-add"
  "C-x C-k C-r"   "kmacro-register"
  "C-x C-k"       "keyboard-macros"
  "C-x RET"       "encoding/input"
  "C-x a i"       "abbrevs-inverse-add"
  "C-x a"         "abbrevs"
  "C-x n"         "narrowing"
  "C-x p"         "projects"
  "C-x r"         "reg/rect/bkmks"
  "C-x t ^"       "tab-bar-detach"
  "C-x t"         "tab-bar"
  "C-x v M"       "vc-mergebase"
  "C-x v b"       "vc-branch"
  "C-x v"         "version-control"
  "C-x w ^"       "window-detach"
  "C-x w"         "window-extras"
  "C-x x"         "buffer-extras"
  "C-x"           "extra-commands"
  "M-g"           "goto-map"
  "M-s h"         "search-highlight"
  "M-s"           "search-map")

;; Org-mode provides some additional prefix-keys in `org-mode-map'.
(with-eval-after-load 'org
  (which-key-add-keymap-based-replacements org-mode-map
    "C-c \""      "org-plot"
    "C-c C-v"     "org-babel"
    "C-c C-x"     "org-extra-commands"))

;; ------- Dired -------
(require 'dired )
(setq dired-listing-switches "-lh")

;; ------- Company -------
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1))

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
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

;; ------- expand-region -------
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; ------- midnight -------
;; At 4:30 in the morning kill any buffers that have not been used in 4 days
;; https://www.emacswiki.org/emacs/MidnightMode
;; By default the ‘midnight-hook’ is configured to just run the CleanBufferList command
(use-package midnight
  :defer 10
  :config
  (midnight-delay-set 'midnight-delay "4:30am")
  :custom
  (clean-buffer-list-delay-general 4))

;; ------- paren-mode -------
(setopt show-paren-delay 0) ; how long to wait?
(show-paren-mode t) ; turn paren-mode on

;; ------- neotree -------
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :ensure t
  :init
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
  :config
  (setopt neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setopt neo-smart-open t))

;; ------- beacon -------
;; Beacon — Never lose your cursor again
;; https://github.com/Malabarba/beacon
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :custom
  (beacon-push-mark 35)
  (beacon-color "#666600"))

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
(setopt backup-inhibited t)
;; Disable auto save files
(setopt auto-save-default nil)
;; Disable lock files - temp symlinks that start with .#
(setopt create-lockfiles nil)

;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    backup-directory-alist
;;    '(("." . "~/.saves"))    ; don't litter my fs tree
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups



;; ------- Save Place -------
(setopt save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(save-place-mode 1)

;; ------- recentf -------
(use-package recentf
  :config
  (recentf-mode +1)
  :custom
  (recentf-save-file "~/.emacs.d/.recentf")
        (recentf-max-saved-items 500)
        (recentf-max-menu-items 25)
        ;; disable recentf-cleanup on Emacs start, because it can cause problems with remote files
        (recentf-auto-cleanup 'never))

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; ------- tramp -------
(setopt tramp-default-method "ssh")
