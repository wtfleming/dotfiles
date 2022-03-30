;; ------- Language Server -------
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((elixir-mode . lsp-deferred)
        (go-mode . lsp-deferred)
        (rust-mode . lsp-deferred)
        (csharp-mode . lsp-deferred)
        (scala-mode . lsp-deferred)
        (clojure-mode . lsp)
        (clojurec-mode . lsp)
        (clojurescript-mode . lsp))
  :init
  (add-to-list 'exec-path "~/bin/elixir-ls")
  :bind (("M-j" . lsp-ui-imenu)
         ("M-?" . lsp-find-references))
  :config
  (setq lsp-file-watch-threshold 2200)
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(push "[/\\\\]\\.vagrant$" lsp-file-watch-ignored)
(push "[/\\\\]\\.circleci$" lsp-file-watch-ignored)

(setq lsp-eldoc-render-all t)
;; (setq lsp-enable-snippet t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
)

(setq lsp-lens-enable t
      lsp-semantic-tokens-enable t
      lsp-ui-doc-enable nil
      lsp-ui-doc-position 'bottom
      lsp-ui-doc-delay 1.0
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

;; ---- LSP Performance ----
;; https://emacs-lsp.github.io/lsp-mode/page/performance/

;; An alternative: follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii: "My suggestion is to repeatedly multiply gc-cons-threshold by 2 until you stop seeing significant improvements in responsiveness, and in any case not to increase by a factor larger than 100 or somesuch. If even a 100-fold increase doesn't help, there's some deeper problem with the Lisp code which produces so much garbage, or maybe GC is not the reason for slowdown."
(setq gc-cons-threshold (* 400 1024 1024)) ;; 400mb like most of the popular starter kits like Spacemacs/Doom/Prelude, etc do

;;(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;; Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; ----- Flycheck -----
;; I use lsp-mode for most programming modes, but still use flycheck for Clojure
(use-package flycheck
  :ensure t)
  ;;:init
  ;;(add-hook 'clojure-mode-hook 'flycheck-mode))

(setq flycheck-checker-error-threshold 1500)

;; ----- Misc -----
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

 ;; Enable trailing whitespace in programming modes
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (set-variable 'show-trailing-whitespace t))))

;; Disable emacs built in version control for faster startup
;; (setq vc-handled-backends ())

(use-package flycheck-inline
  :ensure t)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(add-hook 'go-mode-hook (lambda () (subword-mode +1)))
(add-hook 'elixir-mode-hook (lambda () (subword-mode +1)))
(add-hook 'rust-mode-hook (lambda () (subword-mode +1)))
(add-hook 'csharp-mode-hook (lambda () (subword-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (subword-mode +1)))


;; ----- git-gutter -----
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))


;; ------- magit -------
(use-package magit
             :ensure t
             :bind (("C-c m" . magit-status)))

;; ------- Clojure -------
;; Install a language server
;; brew install clojure-lsp/brew/clojure-lsp-native

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

;; (use-package clojure-mode-extra-font-locking
;;   :ensure t)

(use-package cider
  :ensure t)

(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

;; Syntax highlighting for babashka files
(add-to-list 'interpreter-mode-alist '("bb" . clojure-mode)) 

;; ---- Elixir ----
;; Install a language server
;; Download from https://github.com/elixir-lsp/elixir-ls/releases
;; and unzip it into a directory
;;
;; curl -L https://github.com/elixir-lsp/elixir-ls/releases/latest/download/elixir-ls-1.11.zip --create-dirs -o ~/bin/elixir-ls/elixir-ls.zip
;; cd ~/bin/elixir-ls && unzip elixir-ls.zip

(use-package elixir-mode
  :ensure t)

;; Ignore these directories in Elixir projects
(push "[/\\\\]\\deps$" lsp-file-watch-ignored)
(push "[/\\\\]\\.elixir_ls$" lsp-file-watch-ignored)
(push "[/\\\\]_build$" lsp-file-watch-ignored)

(defvar lsp-elixir--config-options (make-hash-table))
(add-hook 'lsp-after-initialize-hook
          (lambda ()
            (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))

(use-package exunit
  :ensure t)


;; ------- Rust -------
;; Install a language server. Run this command in a terminal
;; $ rustup component add rust-src

;; Next, install rust-analyzer, download a binary from https://github.com/rust-analyzer/rust-analyzer/releases
;; Typically, you then need to rename the binary for your platform, e.g. rust-analyzer-mac if you’re on Mac OS, to rust-analyzer and make it executable in addition to moving it into a directory in your $PATH.
;;
;; $ curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-mac -o ~/bin/rust-analyzer
;; $ chmod +x ~/bin/rust-analyzer

(use-package rust-mode
  :ensure t
  :mode ("\\.rust$" . rust-mode)
  :commands (rust-mode)
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq lsp-rust-server 'rust-analyzer))

(use-package cargo
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; ------- TypeScript -------

;; TypeScript Interactive Development Environment for Emacs
;; https://github.com/ananthakumaran/tide
(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ; (flycheck-select-checker 'typescript-tslint)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Move cursor by camelCase
(add-hook 'typescript-mode-hook (lambda () (subword-mode +1)))

(setq typescript-indent-level 2)



;; ------- JavaScript -------
(use-package js2-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
; (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))


(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (subword-mode +1)))
;;(add-hook 'js2-mode-hook (lambda () (flycheck-mode t)))


;; Tern is a stand-alone, editor-independent JavaScript analyzer that can be used to improve the JavaScript integration of existing editors.
;; https://github.com/ternjs/tern
;; (use-package tern
;;   :ensure t)

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-tern))

;; (add-hook 'js2-mode-hook
;;           (defun my-js2-mode-setup ()
;;             (flycheck-mode t)
;;             (when (executable-find "eslint")
;;               (flycheck-select-checker 'javascript-eslint))))



;; ------- Go -------
(use-package go-mode
  :defer t
  :ensure t
  :mode ("\\.go\\'" . go-mode))


;; go get golang.org/x/tools/gopls
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
(setq lsp-gopls-staticcheck t)
(setq lsp-gopls-complete-unimported t)
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))


;; ------- web-mode -------
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jst.ejs\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.jsx$" . web-mode)
  ("\\.tsx$" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.leex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t))


;; ------- shader-mode -------
;; https://github.com/midnightSuyama/shader-mode
(use-package shader-mode
  :ensure t
    :mode (("\\.shader\\'" . shader-mode)
           ("\\.hlsl\\'" . shader-mode)))

;; ------- yaml-mode -------
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;; ------- lua-mode -------
;; https://github.com/immerrr/lua-mode
(use-package lua-mode
  :ensure t)

;; ------- hcl-mode -------
;; Compatability with HCL and Terraform syntax
(use-package hcl-mode
  :ensure t
  :mode (("\\.tpl\\'" . hcl-mode)
         ("\\.tf\\'" . hcl-mode)))


;; ------- Misc -------
(use-package dockerfile-mode
  :ensure t
  :mode
  (("Dockerfile\\'" . dockerfile-mode)))

;; https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :ensure t)



;; ---- Scala ----
(use-package scala-mode
  :ensure t)
(use-package lsp-metals
  :ensure t)


;; ---- C++ ----
;; To install the language server
;; $ brew install ccls
;; (use-package ccls
;;     :ensure t
;;     :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

;; (add-hook 'c++-mode-hook #'lsp-deferred)

;; ------- C# -------
(use-package csharp-mode
  :ensure t)

;; If the language server was installed manually, can set it here
;;(setq lsp-csharp-server-path "/Users/wtf/bin/omnisharp-osx/run")


;; (defun my-csharp-mode-setup ()
;;   (setq indent-tabs-mode nil)
;;   (setq c-syntactic-indentation t)
;;   (c-set-style "k&r")
;;   (setq c-basic-offset 2)
;;   (setq truncate-lines t)
;;   (setq tab-width 2)
;;   (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
;;   (local-set-key (kbd "C-c C-c") 'recompile))

;; ;; csharp-mode README.md recommends this too
;; ;; (electric-pair-mode 1)       ;; Emacs 24
;; ;; (electric-pair-local-mode 1) ;; Emacs 25

;; (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;; (add-hook 'csharp-mode-hook 'flycheck-mode)
;; (add-hook 'csharp-mode-hook 'omnisharp-mode)


;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-omnisharp))

;; (add-hook 'csharp-mode-hook #'company-mode)

;; ------- Apache Thrift -------
(use-package thrift
  :ensure t)

;; ------- Jenkinsfile -------
(use-package jenkinsfile-mode
  :ensure t)
