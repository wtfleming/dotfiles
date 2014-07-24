;; Scala Mode 2
(add-hook 'scala-mode-hook '(lambda ()
  ;; Bind the 'newline-and-indent' command to RET (aka 'enter').
  ;; This is normally also available as C-j.
  ;; The 'newline-and-indent' command has the following functionality:
  ;; 1) Removes trailing whitespace from the current line.
  ;; 2) Creates a new line.
  ;; 3) Indents it.
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; Bind the 'join-line' command to M-RET. This command is normally
  ;; bound to M-^ which is hard to access.
  ;; The 'join-line' command has the effect or joining the current
  ;; line with the previous while fixing whitespace at the joint.
  (local-set-key (kbd "M-RET") 'join-line)
))

(add-hook 'scala-mode-hook 'color-identifiers-mode)
