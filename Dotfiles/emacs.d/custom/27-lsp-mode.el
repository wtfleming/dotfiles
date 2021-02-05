(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
        (rust-mode . lsp-deferred)
        (scala-mode . lsp-deferred))

(setq lsp-eldoc-render-all t)
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

;; ---- Performance ----
;; https://emacs-lsp.github.io/lsp-mode/page/performance/

;; An alternative: follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii: "My suggestion is to repeatedly multiply gc-cons-threshold by 2 until you stop seeing significant improvements in responsiveness, and in any case not to increase by a factor larger than 100 or somesuch. If even a 100-fold increase doesn't help, there's some deeper problem with the Lisp code which produces so much garbage, or maybe GC is not the reason for slowdown."
(setq gc-cons-threshold 100000000) ;; 100mb like most of the popular starter kits like Spacemacs/Doom/Prelude, etc do

;; Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb


;;  ---- Go----
;; go get golang.org/x/tools/gopls
;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
(setq lsp-gopls-staticcheck t)
(setq lsp-gopls-complete-unimported t)
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))



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

