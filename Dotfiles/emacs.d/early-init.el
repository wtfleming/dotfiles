;;; -*- lexical-binding: t -*-
;; lsp-mode performance optimization
;; see https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

;; A user-lisp file that fails to compile keeps its old .elc; load the newer .el
;; instead, so the error surfaces rather than stale code running.
(setopt load-prefer-newer t)
