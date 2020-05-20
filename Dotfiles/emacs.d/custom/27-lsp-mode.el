(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;;(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-doc-delay 1.0)

;; Run this command in a terminal to install the rust language server
;; rustup component add rls rust-analysis rust-src

(add-hook 'scala-mode-hook #'lsp)

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
(add-to-list 'exec-path "~/bin/elixir-ls")
(add-hook 'elixir-mode-hook #'lsp)


;;(require 'company-lsp)
;;(push 'company-lsp company-backends)

;; (use-package lsp-mode
;;   ;; Optional - enable lsp-mode automatically in scala files
;;   :hook  (scala-mode . lsp)
;;          (lsp-mode . lsp-lens-mode)
;;   :config (setq lsp-prefer-flymake nil))

;; ;; Enable nice rendering of documentation on hover
;; (use-package lsp-ui)



;; (defvar lsp-elixir--config-options (make-hash-table))

;;   (add-hook 'lsp-after-initialize-hook
;;             (lambda ()
;;               (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))

;;(add-to-list 'exec-path "/Users/wtf/src/open-source/elixir-ls/release")



