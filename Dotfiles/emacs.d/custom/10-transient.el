(use-package transient
  :ensure t)

(transient-define-prefix wtf-links ()
  "Common links"
  ["Not defined yet"
    ("w" "Wikipedia random page" (lambda () (interactive) (browse-url "https://en.wikipedia.org/wiki/Special:Random")))]
  [("q" "Quit"           transient-quit-one)])



;; (transient-define-prefix  tutorial-transient ()
;;   "Some Emacs magic"
;;   :info-manual "Surf system-test transient"
;;   ["Not defined yet"
;;     ("p" "print message"      tutorial-print-message)]
;;   [("q" "Quit"           transient-quit-one)])

;; (defun tutorial-print-message (&optional args)
;;   (interactive)
;;   (print "hello world"))


;; (transient-define-prefix tsc-hello ()
;;   "Prefix that is minimal and uses an anonymous command suffix."
;;   [("s" "call suffix"
;;     (lambda ()
;;       (interactive)
;;       (message "Called a suffix")))])
