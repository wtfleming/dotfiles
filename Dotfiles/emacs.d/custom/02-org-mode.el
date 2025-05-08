;; ------- org-reveal -------
;; https://github.com/hexmode/ox-reveal
;; Reveal.js is a tool for creating good-looking HTML presentations.
;; Org-Reveal exports your Org documents to reveal.js presentations.
;; Wait 3 seconds to load as this package is somewhat
;; slow to load, and this helps with emacs startup speed
(use-package ox-reveal
  :defer 3
  :after org
  :ensure t)

;; Can be used for syntax highlighting in org-reveal
(use-package htmlize
  :ensure t)

;; ------- org-babel settings -------
;; Supported languages at https://orgmode.org/worg/org-contrib/babel/languages/index.html
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (js . t)
     ;; (http . t) ;; see https://github.com/zweifisch/ob-http
     (python . t)
     (shell . t))))

;; ------- org-mode settings -------
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setopt org-directory "~/org-mode/")
(setopt org-return-follows-link t)
(setopt org-startup-indented t)


;; ------- key bindings -------
(defvar-keymap wtf-prefix-org-mode-map
  :doc "Prefix key map for org-mode functions I often call."
  "a" #'org-agenda
  "b" #'org-switchb
  "c" #'org-capture
  "l" #'org-store-link)

(defvar-keymap wtf-prefix-map
  :doc "My prefix key map."
  "o" wtf-prefix-org-mode-map)

;; Bind the prefix key map to a key.
;; Notice the absence of a quote for the map's symbol.
(keymap-set global-map "C-c" wtf-prefix-map)

;; Define how the nested keymaps are labelled in `which-key-mode'.
(which-key-add-keymap-based-replacements wtf-prefix-map
  "o" `("org-mode" . ,wtf-prefix-org-mode-map))

;; ------- tags -------
(setopt org-tag-alist
        '(;; Places
          ("@work" . ?w)
          ("@home" . ?h)
          ("laptop" . ?l)

          ;; Activities
          ("@email" . ?e)))

(setopt org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; ------- Org Capture -------
(setopt org-default-notes-file (concat org-directory "/notes.org"))
(setopt org-capture-templates
  '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
      "* TODO %?\n %i\n")
     ("l" "Link" plain (file (concat org-directory "/links.org"))
       "- %?\n %x\n")))

;; ------- Org agenda-------
; Store list of agenda files in org folder so we can easily use
; git to keep it synced
(setopt org-agenda-files (concat org-directory ".agenda-files"))

; Start the agenda on today instead of the monday of this week
(setopt org-agenda-start-on-weekday nil)

; Show two weeks in the agenda view
(setopt org-agenda-span 14)

;; ------- Misc -------
; Don't close windows on exit
(setopt org-agenda-window-setup 'current-window)

; Use solarized CSS for export  http://thomasf.github.io/solarized-css/
(setopt org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)
(setopt org-export-html-style
      (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" (expand-file-name org-directory) "css/solarized-light.min.css\" />"))


;; Use unicode symbol to display org-mode checkboxes
;; https://blog.jft.rocks/emacs/unicode-for-orgmode-checkboxes.html
(add-hook 'org-mode-hook (lambda ()
  "Beautify Org Checkbox Symbol"
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑") prettify-symbols-alist)
  (push '("[-]" . "❍") prettify-symbols-alist)
  (prettify-symbols-mode)))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)
