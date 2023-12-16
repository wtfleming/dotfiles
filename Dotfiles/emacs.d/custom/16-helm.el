(use-package helm
  :ensure t
  :bind (("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-c C-h SPC" . helm-all-mark-rings))
  :config
  (progn
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-M-x-fuzzy-match                  t
          helm-buffers-fuzzy-matching           t
          helm-recentf-fuzzy-match              t)
    (helm-mode 1)))

;;(global-set-key (kbd "C-c h o") 'helm-occur)
;;(global-set-key (kbd "C-c h x") 'helm-register)
;;(global-set-key (kbd "C-c h g") 'helm-google-suggest)


(use-package helm-lsp
  :ensure t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))


;; Allow arrow keys to once again change directories in helm-find-files
;; See https://github.com/emacs-helm/helm/wiki/FAQ#arrow-keys-behavior-have-changed
(customize-set-variable 'helm-ff-lynx-style-map t)

