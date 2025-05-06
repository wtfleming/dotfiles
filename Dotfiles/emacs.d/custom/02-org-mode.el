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

;; ------- Org-mode settings -------
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(setopt org-return-follows-link t)
(setopt org-startup-indented t)

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
(setopt org-directory "~/org-mode/")
(global-set-key "\C-cc" 'org-capture)
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
