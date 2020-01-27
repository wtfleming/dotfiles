(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; Run this command in a terminal to install the rust language server
;; rustup component add rls rust-analysis rust-src
