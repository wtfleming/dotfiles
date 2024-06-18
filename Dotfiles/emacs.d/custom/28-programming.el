;; ------- Language Server -------
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((elixir-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (scala-mode . lsp-deferred)
         (clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (sh-mode . lsp-deferred)
         (yaml-mode . lsp)
         (terraform-mode . lsp-deferred)
        )
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

;; This require is needed to prevent a warning at startup
;; "Symbol's value as variable is void: lsp-file-watch-ignored"
;; TODO look into why it is needed
(require 'lsp-mode)

(push "[/\\\\]\\.vagrant$" lsp-file-watch-ignored)
(push "[/\\\\]\\.circleci$" lsp-file-watch-ignored)

;; (setq lsp-eldoc-render-all t)
;; (setq lsp-enable-snippet t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
)

(setq lsp-lens-enable t
      lsp-semantic-tokens-enable t
      lsp-ui-doc-enable nil
      ;;lsp-ui-doc-position 'bottom
      ;;lsp-ui-doc-delay 1.0
      ;;lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-idle-delay 0.500
      lsp-ui-flycheck-enable t)

;; ---- LSP Performance ----
;; https://emacs-lsp.github.io/lsp-mode/page/performance/

;; Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; ----- Flycheck -----
;; I use lsp-mode for most programming modes, but still use flycheck for Clojure
(use-package flycheck
  :ensure t)
  ;; :init
  ;; (add-hook 'clojure-mode-hook 'flycheck-mode))

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

(add-hook 'c++-mode-hook (lambda () (subword-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (subword-mode +1)))
(add-hook 'csharp-mode-hook (lambda () (subword-mode +1)))
(add-hook 'elixir-mode-hook (lambda () (subword-mode +1)))
(add-hook 'go-mode-hook (lambda () (subword-mode +1)))
(add-hook 'java-mode-hook (lambda () (subword-mode +1)))
(add-hook 'just-mode-hook (lambda () (subword-mode +1)))
(add-hook 'rust-mode-hook (lambda () (subword-mode +1)))
(add-hook 'yaml-mode-hook (lambda () (subword-mode +1)))
(add-hook 'terraform-mode-hook (lambda () (subword-mode +1)))

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
(setq cider-test-show-report-on-success t)

;; Syntax highlighting for babashka files
(add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))

;; ---- Elixir ----
;; Install a language server
;; Download from https://github.com/elixir-lsp/elixir-ls/releases
;; and unzip it into a directory
;;
;; curl -L https://github.com/elixir-lsp/elixir-ls/releases/latest/download/elixir-ls-1.11.zip --create-dirs -o ~/bin/elixir-ls/elixir-ls.zip
;; cd ~/bin/elixir-ls && unzip elixir-ls.zip

;;(setq lsp-elixir-ls-download-url "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.14.6/elixir-ls.zip")

;; https://github.com/elixir-tools/credo-language-server
;; Instructions say to install with this, but it seems to hang downloading from github?
;; for now just download it manually and install to ~/bin (or somewhere else on the shell's path)
;; M-x lsp-install-server credo-language-server
;; (custom-set-variables '(lsp-credo-version "0.3.0"))

(use-package elixir-mode
  :ensure t)

;; Ignore these directories in Elixir projects
(push "[/\\\\]\\deps$" lsp-file-watch-ignored)
(push "[/\\\\]\\.elixir_ls$" lsp-file-watch-ignored)
(push "[/\\\\]_build$" lsp-file-watch-ignored)
(push "[/\\\\]postgres-data$" lsp-file-watch-ignored)

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
;; brew install rust-analyzer

(use-package rust-mode
  :ensure t
  :mode ("\\.rust$" . rust-mode)
  :commands (rust-mode)
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq lsp-rust-server 'rust-analyzer)
  (setq-default lsp-rust-analyzer-proc-macro-enable t))

;; (use-package cargo
;;   :ensure t
;;   :after rust-mode
;;   :config
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode))

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

;; go install golang.org/x/tools/gopls@latest
;; and ensure $HOME/go/bin is in the shell's path
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
  :ensure t)

;; ------- terraform-mode -------
(use-package terraform-mode
  :ensure t
  :mode (("\\.tpl\\'" . terraform-mode)
         ("\\.tf\\'" . terraform-mode))

  ;; Currently lsp-mode supports two terraform language servers.
  ;; If you would want to go with the official Hashicorp's language server, set this:
  :config (setq lsp-disabled-clients '(tfls)))


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
;; (use-package lsp-metals
;;   :ensure t)


;; ---- C++ ----
;; To install the clangd language server
;; $ brew install llvm
(add-hook 'c++-mode-hook #'lsp-deferred)
;;(add-hook 'c++-mode-hook (semantic-mode -1))

;; Remove company-semantic from the backends to make autocompete work in C++
;; Want to be using company-capf as the backend
(defun my-c++-mode-hook ()
  (setq-local company-backends (delete 'company-semantic company-backends)))

(add-hook 'c++-mode-hook #'my-c++-mode-hook)

;; ---- CMake ----
(use-package cmake-mode
  :ensure t)

;; ------- Apache Thrift -------
;; (use-package thrift
;;   :ensure t)

;; ------- Jenkinsfile -------
(use-package jenkinsfile-mode
  :ensure t)

;; ------- Java -------
;; indent 2 spaces
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 2)))

;; ------- GraphQL -------
(use-package graphql-mode
  :ensure t)

;; needed by graphql-mode
(use-package request
  :ensure t)

;; ------- https://github.com/casey/just -------
(use-package just-mode
  :ensure t)

;; ------- https://github.com/psibi/justl.el -------
(use-package justl
  :ensure t)

;; ------- https://github.com/abrochard/mermaid-mode -------
;; mermaid-js charts
(use-package mermaid-mode
  :ensure t)
