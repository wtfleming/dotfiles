(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
        (rust-mode . lsp-deferred)
        (scala-mode . lsp-deferred))

;; (setq lsp-enable-snippet t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
)

(setq lsp-ui-doc-enable nil
      lsp-ui-doc-position 'bottom
      lsp-ui-doc-delay 1.0
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

;; ---- Rust ----
;; Run this command in a terminal to install the rust language server:
;; rustup component add rls rust-analysis rust-src


;;  ---- Go----
;; go get golang.org/x/tools/gopls
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
;;(add-hook 'go-mode-hook #'lsp-deferred)
(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)



;; ---- Elixir ----
;; Clone https://github.com/elixir-lsp/elixir-ls
;; cd elixir-ls (that you just cloned)
;; mix deps.get
;; mix elixir_ls.release -o ~/bin/elixir-ls
;;
;; If using asdf and a recent macos (ie Catalina) you may need to use a more recent OTP to build
;; change the .tool-versions to look something like this, then asdf install
;; Then run asdf global for elixir and erlang
;; elixir 1.10.3-otp-23
;; erlang 23.0
;; (add-to-list 'exec-path "~/bin/elixir-ls")
;; (add-hook 'elixir-mode-hook #'lsp-deferred)

;; ;; Ignore these directories in elixir projects
;; (push "[/\\\\]\\deps$" lsp-file-watch-ignored)
;; (push "[/\\\\]\\.elixir_ls$" lsp-file-watch-ignored)
;; (push "[/\\\\]_build$" lsp-file-watch-ignored)

;; (defvar lsp-elixir--config-options (make-hash-table))
;; (add-hook 'lsp-after-initialize-hook
;;           (lambda ()
;;             (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))


;; ---- Scala ----
;; (add-hook 'scala-mode-hook #'lsp-deferred)


;; ---- C++ ----
;; To install the language server
;; $ brew install ccls
;; (use-package ccls
;;     :ensure t
;;     :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

;; (add-hook 'c++-mode-hook #'lsp-deferred)
