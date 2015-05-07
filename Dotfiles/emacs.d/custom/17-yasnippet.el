; yasnippet

; cd ~/.emacs.d
; git clone git@github.com:AndreaCrotti/yasnippet-snippets.git


(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "~/.emacs.d/yasnippet-snippets"       ;; https://github.com/AndreaCrotti/yasnippet-snippets
        ))

(yas-global-mode 1)
