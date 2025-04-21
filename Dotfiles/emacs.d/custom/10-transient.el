(use-package transient
  :ensure t)

(transient-define-prefix  tutorial-transient ()
  "Some Emacs magic"
  :info-manual "Surf system-test transient"
  ["Not defined yet"
    ("p" "print message"      tutorial-print-message)]
  [("q" "Quit"           transient-quit-one)])

(defun tutorial-print-message (&optional args)
  (interactive)
  (print "hello world"))
