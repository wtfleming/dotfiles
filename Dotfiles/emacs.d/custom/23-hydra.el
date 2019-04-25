; Config for https://github.com/abo-abo/hydra

;; (defhydra hydra-zoom (global-map "<f2>")
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))


;; (defhydra hydra-yank-pop ()
;;   "yank"
;;   ("C-y" yank nil)
;;   ("M-y" yank-pop nil)
;;   ("y" (yank-pop 1) "next")
;;   ("Y" (yank-pop -1) "prev")
;;   ("l" helm-show-kill-ring "list" :color blue))   ; or browse-kill-ring
;; (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
;; (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)


; TODO there are two helps defined here, choose one to keep

; Help
(setq my-default-hydra-delay 0.5)

(defhydra hydra-metahelp-menu (:hint nil :exit t :idle my-default-hydra-delay :foreign-keys warn)
  "
Describe                           ^^^^^^                             Goto         ^^ View
-----------------------------------------------------------------------------------------------------
_b_indings             _k_ey                   _s_ymbol               _e_:*Messages*  _a_propos
_c_:key-briefly        _K_ey (info)            _S_ymbol (info)        _i_nfo manual   _l_ossage
_C_oding system        _L_anguage environment  _C-s_yntax table ^     _._:local help
_d_ocumentation        _m_ode                  _v_ariable
_E_macs...             _p_ackage (by topic)    _V_ariable (counsel)
_f_unction             _P_ackage (by name)     _w_hereis (func->keys)
_F_unction (info)      _C-p_: external package
_I_:key input method                                           ^^^^^^                 _q_uit
"
  ("?" counsel-hydra-heads)
  ("a"   counsel-apropos-thing-at-point)
  ("b"   describe-bindings)
  ("c"   describe-key-briefly)
  ("C"   describe-coding-system)
  ("d"   apropos-documentation)
  ("e"   view-echo-area-messages)
  ("E"   hydra-metahelp-emacs-menu/body)
  ("f"   counsel-describe-function)
  ("F"   Info-goto-emacs-command-node)
  ("i"   info)
  ("I"   describe-input-method)
  ("k"   describe-key)
  ("K"   Info-goto-emacs-key-command-node)
  ("l"   view-lossage)
  ("L"   describe-language-environment)
  ("m"   describe-mode)
  ("p"   finder-by-keyword)
  ("P"   describe-package)
  ("C-p" view-external-packages)
  ("q"   nil nil)
  ("s"   describe-symbol)
  ("S"   info-lookup-symbol)
  ("C-s" describe-syntax)
  ("v"   describe-variable)
  ("V"   counsel-describe-variable)
  ("w"   where-is)
  ("."   display-local-help))

(defhydra hydra-metahelp-emacs-menu (:hint nil :exit t :idle my-default-hydra-delay :foreign-keys warn)
  "
Emacs
----------------------------------------------------------------------------------------
_a_bout Emacs  _D_istribution  _h_ello file     _n_ews            _T_odo          _q_uit
_c_opying      _F_AQ           _i_nfo manual    known _p_roblems  no _w_arranty
_d_ebuging     _G_NU           order _m_anuals  _t_utorial
"
  ("?" counsel-hydra-heads)
  ("a" about-emacs)
  ("c" describe-copying)
  ("d" view-emacs-debugging)
  ("D" describe-distribution)
  ("F" view-emacs-FAQ)
  ("G" describe-gnu-project)
  ("h" view-hello-file)
  ("i" info-manual)
  ("n" view-emacs-news)
  ("q" nil nil)
  ("t" help-with-tutorial)
  ("m" view-order-manuals)
  ("p" view-emacs-problems)
  ("T" view-emacs-todo)
  ("w" describe-no-warranty))


(global-set-key (kbd "C-h") #'hydra-metahelp-menu/body)



; Help
(defhydra hydra-help (:exit t)
    ;; Better to exit after any command because otherwise helm gets in a
    ;; mess, set hint to nil: written out manually.

    "
  Describe        ^^Keys                    ^^Search                    ^^Documentation
  ---------------------------------------------------------------------------------------
  _f_unction        _k_eybinding              _a_propros                  _i_nfo
  _p_ackage         _w_here-is                _d_oc strings               _n_: man
  _m_ode            _b_: show all bindings    _s_: info by symbol         
  _v_ariable

  "
    ;; Boring help commands...
    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("C" describe-coding-system "coding-system")
    ("I" describe-input-method "input-method")


    ;; Documentation
    ("i" info nil)
    ("n" helm-man-woman nil)

    ;; Keybinds
    ("b" describe-bindings nil)
    ("c" describe-key-briefly nil)
    ("k" describe-key nil)
    ("w" where-is nil)

    ;; Search
    ("a" apropos-command nil)
    ("d" apropos-documentation nil)
    ("s" info-lookup-symbol nil)

    ;; Describe
    ("f" describe-function nil)
    ("p" describe-package nil)
    ("m" describe-mode nil)
    ("v" describe-variable nil)
    ("y" describe-syntax nil)

    ;; quit
    ("q" help-quit "quit"))
  (global-set-key (kbd "<f1>") #'hydra-help/body)



;; ; Movement
;; (global-set-key
;;  (kbd "C-n")
;;  (defhydra hydra-move
;;    (:body-pre (next-line))
;;    "move"
;;    ("n" next-line)
;;    ("p" previous-line)
;;    ("f" forward-char)
;;    ("b" backward-char)
;;    ("a" beginning-of-line)
;;    ("e" move-end-of-line)
;;    ("v" scroll-up-command)
;;    ;; Converting M-v to V here by analogy.
;;    ("V" scroll-down-command)
;;    ("l" recenter-top-bottom)))


;; ; Transpose
;; (global-set-key (kbd "C-c m")
;;                 (defhydra hydra-transpose (:color red)
;;                   "Transpose"
;;                   ("c" transpose-chars "characters")
;;                   ("w" transpose-words "words")
;;                   ("o" org-transpose-words "Org mode words")
;;                   ("l" transpose-lines "lines")
;;                   ("s" transpose-sentences "sentences")
;;                   ("e" org-transpose-elements "Org mode elements")
;;                   ("p" transpose-paragraphs "paragraphs")
;;                   ("t" org-table-transpose-table-at-point "Org mode table")
;;                   ("q" nil "cancel" :color blue)))







; Dired
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)


(defhydra hydra-projectile (:color teal
			    :columns 4)
  "Projectile"
  ("f"   helm-projectile-find-file           "Find File")
  ("F"   helm-projectile-find-file-dwim      "Find File dwim")
  ("g"   helm-projectile-grep                "grep")
  ("r"   helm-projectile-recentf             "Recent Files")

  ("z"   projectile-cache-current-file       "Cache Current File")
  ("d"   helm-projectile-find-dir            "Find Directory")
  ("b"   helm-projectile-switch-to-buffer    "Switch to Buffer")
  ("c"   projectile-invalidate-cache         "Clear Cache")

  ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")
  ("o"   projectile-multi-occur              "Multi Occur")
  ("s"   helm-projectile-switch-project      "Switch Project")
  ("k"   projectile-kill-buffers             "Kill Buffers")

  ("q"   nil "Cancel" :color blue))


(define-key projectile-mode-map (kbd "C-c p") 'hydra-projectile/body)
(define-key projectile-mode-map (kbd "s-p") 'hydra-projectile/body)
