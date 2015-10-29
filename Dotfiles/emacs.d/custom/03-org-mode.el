;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-return-follows-link t)
(setq org-startup-indented t)

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; Org Capture
(setq org-directory "~/org-mode/")
(global-set-key "\C-cc" 'org-capture)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
  '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
      "* TODO %?\n %i\n")
     ("l" "Link" plain (file (concat org-directory "/links.org"))
       "- %?\n %x\n")))

;; Org agenda
; Store list of agenda files in org folder so we can easily use
; git to keep it synced
(setq org-agenda-files (concat org-directory ".agenda-files"))

; Start the agenda on today instead of the monday of this week
(setq org-agenda-start-on-weekday nil)

; Show two weeks in the agenda view
(setq org-agenda-span 14)

;; Misc
; Don't close windows on exit
(setq org-agenda-window-setup 'current-window)

; Use solarized CSS for export  http://thomasf.github.io/solarized-css/
(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)
(setq org-export-html-style
      (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" (expand-file-name org-directory) "css/solarized-light.min.css\" />"))

