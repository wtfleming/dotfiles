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
         (typescript-ts-mode . lsp-deferred)
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

;; TODO this could/should be in an :after in the use-package expression above?
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.vagrant\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.circleci\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\deps$") ;; Elixir
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build$") ;; Elixir
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]postgres-data$")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.vagrant\\'"))

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
;; TODO look into if I still need flycheck, am I actually using it
;;   or is lsp-mode doing enough?
;;   ie see https://github.com/emacs-lsp/lsp-mode/issues/318
(use-package flycheck
  :ensure t)
  ;; :init
  ;; (add-hook 'clojure-mode-hook 'flycheck-mode))

(setq flycheck-checker-error-threshold 1500)


;; ----- tree-sitter -----
;; TODO look at https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

(use-package treesit
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ;;("\\.mjs\\'" . typescript-ts-mode)
             ;;("\\.mts\\'" . typescript-ts-mode)
             ;;("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ;;("\\.json\\'" .  json-ts-mode)
             ;;("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ;;("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '(;;(python-mode . python-ts-mode)
                 ;;(css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 ;;(c-mode . c-ts-mode)
                 ;;(c++-mode . c++-ts-mode)
                 ;;(c-or-c++-mode . c-or-c++-ts-mode)
                 ;;(bash-mode . bash-ts-mode)
                 ;;(css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 ;;(sh-mode . bash-ts-mode)
                 ;;(sh-base-mode . bash-ts-mode)
                 ))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))

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


;; TODO also need to do this for anything using tree siter
;;   for example typescript-ts-mode
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
(add-hook 'typescript-mode-hook (lambda () (subword-mode +1)))
(add-hook 'js2-mode-hook (lambda () (subword-mode +1)))


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

(defvar lsp-elixir--config-options (make-hash-table))
(add-hook 'lsp-after-initialize-hook
          (lambda ()
            (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))

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
;; See
;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; ;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)



;; ------- JavaScript -------
;; TODO do I still need this package?
(use-package js2-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
; (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))


;; ------- Go -------
(use-package go-mode
  :defer t
  :ensure t
  :mode ("\\.go\\'" . go-mode))

;; go install golang.org/x/tools/gopls@latest
;; and ensure $HOME/go/bin is in the shell's path
(setq lsp-gopls-staticcheck t)
(setq lsp-gopls-complete-unimported t)
;; (lsp-register-custom-settings
;;  '(("gopls.completeUnimported" t t)
;;    ("gopls.staticcheck" t t)))


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
;; (use-package lua-mode
;;   :ensure t)

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
;; TODO is this still useful, or do I just use this now?
;;   M-x lsp-format-buffer
(use-package format-all
  :ensure t)


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
;; (use-package jenkinsfile-mode
;;   :ensure t)

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
;; (use-package just-mode
;;   :ensure t)

;; ------- https://github.com/psibi/justl.el -------
;; (use-package justl
;;   :ensure t)

;; ------- https://github.com/abrochard/mermaid-mode -------
;; mermaid-js charts
(use-package mermaid-mode
  :ensure t)
