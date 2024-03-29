(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'ibuf-ext)
;; Hide helm buffers
(add-to-list 'ibuffer-never-show-predicates "^\\*[Hh]elm")

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
               ("Programming"
                (or
                 (mode . clojure-mode)
                 (mode . conf-toml-mode)
                 (mode . elixir-mode)
                 (mode . emacs-lisp-mode)
                 (mode . java-mode)
                 (mode . go-mode)
                 (mode . pig-mode)
                 (mode . python-mode)
                 (mode . rust-mode)
                 (mode . scala-mode)
                 (mode . thrift-mode)
                 (mode . web-mode)
                 ))
               ("org-mode" (mode . org-mode))
               ;; ("org-agenda" (or
               ;;                (mode . org-agenda-mode)
               ;;                (predicate . (my-org-agenda-filter))))
               ("Dired" (mode . dired-mode))
               ("erc" (mode . erc-mode))
               ("Markdown" (mode . markdown-mode))
;               ("helm" (or
;                        (name . "^\\*helm")
;                        (name . "^\\*Helm")))

               ("Emacs" (or
                         (mode . package-menu-mode)
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*Messages\\*$")))
               ("Magit" (or
                         (name . "^magit-")
                         (mode . magit-status-mode)))
               ))))


;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

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

;; With this, when you press 'up' or 'down' to the top/bottom of IBuffer,
;; the cursor wraps around to the bottom/top, so you can continue from there.
(defun ibuffer-previous-line ()
  (interactive) (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (count-lines (point-min) (point-max)))))
(defun ibuffer-next-line ()
  (interactive) (next-line)
  (if (>= (line-number-at-pos) (+ (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))
(define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)
