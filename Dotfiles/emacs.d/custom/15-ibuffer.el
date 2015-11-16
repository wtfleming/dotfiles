(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Show org-agenda files in own group
;; http://emacs.stackexchange.com/questions/2087/predicates-in-ibuffer-saved-filter-groups
(defun my-org-agenda-filter ()
  (let ((fname (buffer-file-name)))
    (and fname
         (member (file-truename fname)
                 (mapcar 'file-truename (org-agenda-files))))))

;; Show groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ;; ("org-agenda" (or
               ;;                (mode . org-agenda-mode)
               ;;                (predicate . (my-org-agenda-filter))))
               ("dired" (mode . dired-mode))
               ("erc" (mode . erc-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*Messages\\*$")))
               ("org-mode" (mode . org-mode))
               ("Programming"
                (or
                 (mode . pig-mode)
                 (mode . clojure-mode)
                 (mode . scala-mode)
                 (mode . python-mode)
                 (mode . emacs-lisp-mode)
                 ;; etc
                 ))
               ))))


(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))




;; ---------- display --------------------------------
;; Display human readable buffer sizes
(define-ibuffer-column size-h
  (:name "Size")
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 34 34 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              filename-and-process)))

;; --------------------------------------


;; Switching to ibuffer puts the cursor on the most recent buffer
  (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
  (ad-activate 'ibuffer)

;; Hide the summary at the bottom of the buffer
(setq ibuffer-display-summary nil)
