;; Get current system type
(defun insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type)))

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))


(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun wtf-googlemaps-lat-lon ()
  "Open in google maps the selected region if any, otherwise display a query prompt. Expects lat/lon pair to be whitespace separated"
  (interactive)
  (let* ((lat-lon-string (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Googlemaps lat/lon: "))))
         (lat-lon-list (split-string lat-lon-string))
         (lat (nth 0 lat-lon-list))
         (lon (nth 1 lat-lon-list))
         (url-params (concat lat "%2C" lon)))
    (browse-url
     (concat "http://maps.google.com/?q=" lat))))

(defun wtf-json-pretty-print ()
  "JSON pretty print the selected region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[a-zA-Z0-9_\-]+" "\\&" beg end))
