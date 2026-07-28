;;; wtf-github.el --- Code to interact with GitHub                -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Will Fleming

;; Author: Will Fleming <wfleming77@gmail.com>
;; Keywords: tools
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.2") (transient "0.9.0"))

;;; Commentary:

;; Browse GitHub from Emacs by shelling out to the `gh' command line
;; tool.
;;
;; `C-c G' opens a transient dispatch for choosing the active
;; organization and repository, and for listing that repository's open
;; pull requests or issues.  The dispatch also carries filter switches
;; (only mine, exclude drafts, review requested).
;;
;; A listing is a `vtable'.  RET opens the item in the browser, w
;; copies its URL, d shows a pull request's diff, S sorts by the column
;; at point, g refetches, and q closes the listing.
;;
;; Pull requests and issues are fetched fresh every time, in the
;; background, so Emacs stays responsive.  Repo lists are cached per
;; owner, since that `gh' query is slow and the set of repositories
;; rarely changes; `wtf-clear-repo-cache' discards them.

;;; Code:
(require 'map)
(require 'transient)
(require 'vtable)


;;;; Customization

(defgroup wtf-github nil
  "Browse GitHub using the `gh' command line tool."
  :group 'tools
  :prefix "wtf-")

(defcustom wtf-username nil
  "GitHub login used as the owner when no organization is active.
Nil means ask `gh' for the authenticated user, once per session."
  :type '(choice (const :tag "Ask gh" nil) (string :tag "Login"))
  :group 'wtf-github)

(defcustom wtf-repo-limit 1000
  "Maximum number of repositories to request from `gh'."
  :type 'natnum
  :group 'wtf-github)

(defcustom wtf-item-limit 100
  "Maximum number of pull requests or issues to request from `gh'."
  :type 'natnum
  :group 'wtf-github)


;;;; Running gh

(defun wtf--gh-program ()
  "Return the `gh' executable.
Signal an error when it is missing from the variable `exec-path', which
differs from the shell PATH when Emacs is started from a GUI."
  (or (executable-find "gh")
      (user-error "Cannot find gh; check the variable `exec-path'")))

(defun wtf--gh (&rest args)
  "Run gh with ARGS and return its output as a string.
Signal a `user-error' carrying gh's own message when it fails, rather
than letting the caller trip over unparseable output."
  (with-temp-buffer
    (let ((status (apply #'call-process (wtf--gh-program) nil t nil args)))
      (unless (zerop status)
        (user-error "Command gh %s failed: %s" (string-join args " ")
                    (string-trim (buffer-string))))
      (buffer-string))))

(defun wtf--parse-json (output args)
  "Parse OUTPUT as JSON, blaming the gh invocation ARGS on failure."
  (condition-case nil
      (json-parse-string output :null-object nil)
    (json-parse-error
     (user-error "Command gh %s returned unexpected output: %s"
                 (string-join args " ") (string-trim output)))))

(defun wtf--gh-json (&rest args)
  "Run gh with ARGS and return its output parsed as JSON."
  (wtf--parse-json (apply #'wtf--gh args) args))

(defun wtf--gh-json-async (args callback)
  "Run gh with ARGS in the background, calling CALLBACK with parsed JSON.
Failures are reported in the echo area and CALLBACK is not called."
  (let ((buf (generate-new-buffer " *wtf-gh*")))
    (make-process
     :name "wtf-gh"
     :buffer buf
     :noquery t
     ;; A pipe, not the default pty: on a tty gh pretty-prints its JSON and
     ;; wraps it in ANSI colour codes, which `json-parse-string' rejects.
     :connection-type 'pipe
     :command (cons (wtf--gh-program) args)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((status (process-exit-status proc))
               (output (with-current-buffer buf (buffer-string))))
           (kill-buffer buf)
           (cond
            ((not (zerop status))
             (message "Command gh %s failed: %s" (string-join args " ") (string-trim output)))
            (t
             (let ((json (condition-case nil
                             (json-parse-string output :null-object nil)
                           (json-parse-error :wtf-unparseable))))
               (if (eq json :wtf-unparseable)
                   (message "Command gh %s returned unexpected output: %s"
                            (string-join args " ") (string-trim output))
                 (funcall callback json)))))))))))


;;;; Owner: the user, or an organization

(defvar wtf--active-org nil
  "Organization whose repos `wtf-read-repo' offers.
Nil means the personal account returned by `wtf--username'.
Set it with `wtf-set-active-org'; read it with `wtf-active-owner'.")

(defvar wtf--username-cache nil
  "Login of the authenticated user, as reported by `gh'.")

(defun wtf--username ()
  "Return the authenticated GitHub login.
Uses `wtf-username' when set, otherwise asks gh once per session."
  (or wtf-username
      wtf--username-cache
      (setq wtf--username-cache
            (string-trim (wtf--gh "api" "user" "--jq" ".login")))))

(defun wtf-active-owner ()
  "Return the owner repos are listed for: the active org, else the user."
  (or wtf--active-org (wtf--username)))

(defun wtf-request-list-orgs ()
  "List organizations the authenticated user belongs to."
  (split-string (wtf--gh "org" "list") "\n" t))

(defun wtf-read-org (prompt)
  "Prompt the user with PROMPT for an org, offering the personal account too."
  (completing-read prompt (cons (wtf--username) (wtf-request-list-orgs)) nil t))


;;;; Repositories

(defvar wtf--repo-cache nil
  "Alist mapping an owner to its cached list of repos.
The `gh' call is slow, so each owner is fetched at most once per
session and switching back to a previous owner is free.
Use `wtf-clear-repo-cache' to discard every entry.")

(defvar wtf--last-repo-history nil
  "Minibuffer history of repos read by `wtf-read-repo'.")

(defun wtf-clear-repo-cache ()
  "Discard all cached repo lists so the next read refetches."
  (interactive)
  (setq wtf--repo-cache nil)
  (message "Repo cache cleared"))

(defun wtf-request-list-repos ()
  "Request non-archived repos owned by `wtf-active-owner'."
  (wtf--gh-json "repo" "list" (wtf-active-owner)
                "--no-archived"
                "-L" (number-to-string wtf-repo-limit)
                "--json" "nameWithOwner"))

(defun wtf-list-repos ()
  "List non-archived repos for `wtf-active-owner' as \"owner/name\" strings.
Warn when the result reaches `wtf-repo-limit', since gh truncates
without saying so."
  (let ((repos (wtf-request-list-repos)))
    (when (>= (length repos) wtf-repo-limit)
      (message "Warning: reached wtf-repo-limit (%d); some repos are missing"
               wtf-repo-limit))
    (seq-map (lambda (e) (map-elt e "nameWithOwner")) repos)))

(defun wtf-read-repo (prompt)
  "Prompt the user with PROMPT, defaulting to the last repo read.
Candidates come from `wtf-active-owner' and are cached per owner in
`wtf--repo-cache'.  A repo outside that list is accepted but must be
confirmed, which catches typos while still allowing other repos."
  (let* ((owner (wtf-active-owner))
         (repo-list (or (alist-get owner wtf--repo-cache nil nil #'equal)
                        (let ((repos (wtf-list-repos)))
                          (push (cons owner repos) wtf--repo-cache)
                          repos))))
    (completing-read prompt repo-list nil 'confirm nil
                     'wtf--last-repo-history
                     (car wtf--last-repo-history))))


;;;; Active repo and org

(defvar wtf--active-repo nil
  "Repo that repo-scoped commands act on, as \"owner/name\".
Set it with `wtf-set-active-repo'; read it with `wtf-active-repo'.")

(defun wtf-set-active-repo (repo)
  "Make REPO the active repo and return it."
  (interactive (list (wtf-read-repo "Active repo: ")))
  (setq wtf--active-repo repo)
  (message "Active repo: %s" repo)
  repo)

(defun wtf-active-repo ()
  "Return the active repo, prompting to pick one if none is set."
  (or wtf--active-repo
      (wtf-set-active-repo (wtf-read-repo "Active repo: "))))

(defun wtf-set-active-org (org)
  "Make ORG the owner whose repos are offered, and return it.
Discards the active repo, which belonged to the previously active owner.
Cached repo lists are kept per owner, so switching back is fast."
  (interactive (list (wtf-read-org "Active org: ")))
  (setq wtf--active-org org)
  (setq wtf--active-repo nil)
  (message "Active org: %s" org)
  org)


;;;; Presentation

(defface wtf-list-row '((t))
  "Face for ordinary rows in a listing.
Deliberately empty; it is the unstriped half of `:row-colors'."
  :group 'wtf-github)

(defface wtf-list-row-alt
  '((((background light)) :background "#f2f2f2")
    (((background dark))  :background "#2b2b2b"))
  "Face for alternating rows in a listing.
Sets only a background, so the CI column keeps its own foreground."
  :group 'wtf-github)

(defun wtf--format-age (seconds)
  "Format SECONDS, a Unix timestamp, as a compact age such as \"3h\"."
  (let ((age (- (float-time) seconds)))
    (cond ((< age 3600)  (format "%dm" (truncate (/ age 60))))
          ((< age 86400) (format "%dh" (truncate (/ age 3600))))
          (t             (format "%dd" (truncate (/ age 86400)))))))

(defun wtf--author (item)
  "Return the login of ITEM's author."
  (map-elt (map-elt item "author") "login"))

;; A timestamp rather than a formatted string: vtable sorts on the getter's
;; value, so "10m" would sort before "3h" alphabetically.
(defun wtf--updated-at (item)
  "Return ITEM's update time as a Unix timestamp."
  (float-time (date-to-time (map-elt item "updatedAt"))))

(defun wtf--ci-status (pr)
  "Summarize CI state for PR as a short coloured string.
CheckRun entries report `conclusion', older StatusContext entries
report `state', so both are consulted.  A run still in flight has an
empty conclusion, hence the separate `status' test."
  (let* ((checks (append (map-elt pr "statusCheckRollup") nil))
         (result (lambda (c) (or (map-elt c "conclusion") (map-elt c "state"))))
         (failed (seq-count
                  (lambda (c) (member (funcall result c)
                                      '("FAILURE" "ERROR" "TIMED_OUT"
                                        "CANCELLED" "ACTION_REQUIRED")))
                  checks))
         (running (seq-count
                   (lambda (c) (or (member (map-elt c "status")
                                           '("QUEUED" "IN_PROGRESS" "WAITING"))
                                   (equal (funcall result c) "PENDING")))
                   checks)))
    (cond ((null checks)  (propertize "-" 'face 'shadow))
          ((> failed 0)   (propertize (format "✗ %d" failed) 'face 'error))
          ((> running 0)  (propertize (format "● %d" running) 'face 'warning))
          (t              (propertize "✓" 'face 'success)))))


;;;; Listings

(defun wtf--kind-subcommand (kind)
  "Return the gh subcommand for KIND, either `pr' or `issue'."
  (if (eq kind 'pr) "pr" "issue"))

(defun wtf--kind-label (kind)
  "Return a human readable plural label for KIND."
  (if (eq kind 'pr) "PRs" "issues"))

(defun wtf--kind-buffer (kind repo)
  "Return the listing buffer name for KIND on REPO."
  (format "*wtf-github-%s: %s*" (if (eq kind 'pr) "prs" "issues") repo))

(defun wtf--list-command (kind repo args)
  "Return the gh argument list for listing KIND on REPO with filters ARGS.
`--draft' is dropped for issues, where gh does not accept it."
  (append (list (wtf--kind-subcommand kind) "list"
                "--repo" repo
                "--state" "open"
                "-L" (number-to-string wtf-item-limit)
                "--json" (if (eq kind 'pr)
                             "number,title,author,updatedAt,statusCheckRollup,url"
                           "number,title,author,updatedAt,url"))
          (if (eq kind 'pr)
              args
            (seq-remove (lambda (a) (string-prefix-p "--draft" a)) args))))

(defun wtf--list-columns (kind)
  "Return the vtable column specs for KIND."
  (if (eq kind 'pr)
      '((:name "PR" :align right)
        (:name "CI")
        (:name "Updated" :align right :formatter wtf--format-age)
        (:name "Author")
        (:name "Title"))
    '((:name "#" :align right)
      (:name "Updated" :align right :formatter wtf--format-age)
      (:name "Author")
      (:name "Title"))))

(defun wtf--list-getter (kind)
  "Return the vtable getter for KIND."
  (if (eq kind 'pr)
      (lambda (item column _table)
        (pcase column
          (0 (map-elt item "number"))
          (1 (wtf--ci-status item))
          (2 (wtf--updated-at item))
          (3 (wtf--author item))
          (4 (map-elt item "title"))))
    (lambda (item column _table)
      (pcase column
        (0 (map-elt item "number"))
        (1 (wtf--updated-at item))
        (2 (wtf--author item))
        (3 (map-elt item "title"))))))

(defun wtf-list-quit ()
  "Kill the listing and restore the previous window layout."
  (interactive)
  (quit-window t))

(defun wtf-kill-list-buffers ()
  "Kill every wtf-github listing and diff buffer."
  (interactive)
  (let ((n 0))
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*wtf-github-" (buffer-name buf))
        (kill-buffer buf)
        (setq n (1+ n))))
    (message "Killed %d wtf-github buffer%s" n (if (= n 1) "" "s"))))

(defun wtf-show-pr-diff (repo number)
  "Show the diff of pull request NUMBER on REPO in a `diff-mode' buffer."
  (let ((buf (get-buffer-create (format "*wtf-github-diff: %s#%s*" repo number))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (wtf--gh "pr" "diff" (number-to-string number) "--repo" repo)))
      (diff-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun wtf--list-actions (kind repo args)
  "Return the vtable :actions list for KIND on REPO with filters ARGS."
  (append
   (list "RET" (lambda (item) (browse-url (map-elt item "url")))
         "w"   (lambda (item)
                 (kill-new (map-elt item "url"))
                 (message "Copied %s" (map-elt item "url")))
         ;; `vtable-map' binds g to `vtable-revert-command', which only
         ;; redraws the cached objects.  Its keymap is a text property on
         ;; every row, so it shadows `special-mode's g; override it here.
         "g"   (lambda (_item) (wtf--show-items kind repo args)))
   (when (eq kind 'pr)
     (list "d" (lambda (item)
                 (wtf-show-pr-diff repo (map-elt item "number")))))))

(defun wtf--goto-item (number)
  "Move point to the row for item NUMBER.
Return non-nil when a matching row was found."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (eql number (map-elt (vtable-current-object) "number"))
        (throw 'found t))
      (forward-line 1))
    nil))

(defun wtf--render-items (kind repo args items)
  "Render ITEMS of KIND for REPO into its listing buffer.
ARGS are the gh filter switches that produced ITEMS; they are kept so
that g refetches with the same filters."
  (let* ((buf (get-buffer-create (wtf--kind-buffer kind repo)))
         ;; Remember the item under point, not the line: a refetch can add
         ;; or drop rows, and restoring by line would silently land on a
         ;; different item.  Keep the sort order too, so g does not undo S.
         (prev (with-current-buffer buf
                 (map-elt (vtable-current-object) "number")))
         (sort-by (with-current-buffer buf
                    (save-excursion
                      (goto-char (point-min))
                      (when-let ((table (vtable-current-table)))
                        (vtable-sort-by table))))))
    (with-current-buffer buf
      (special-mode)
      ;; Compose rather than mutate: `special-mode-map' is shared.
      (use-local-map (make-composed-keymap
                      (define-keymap "q" #'wtf-list-quit)
                      special-mode-map))
      (setq-local revert-buffer-function
                  (lambda (&rest _) (wtf--show-items kind repo args)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (zerop (length items))
            (insert (format "No open %s.\n" (wtf--kind-label kind)))
          (make-vtable
           :columns (wtf--list-columns kind)
           :objects (append items nil)
           :row-colors '(wtf-list-row wtf-list-row-alt)
           :sort-by sort-by
           :actions (wtf--list-actions kind repo args)
           :getter (wtf--list-getter kind))))
      (unless (and prev (wtf--goto-item prev))
        (goto-char (point-min))))
    ;; Outside `with-current-buffer': its `save-current-buffer' would
    ;; otherwise undo the selection when the form exits.
    (pop-to-buffer buf)
    (message "Fetching %s on %s...done (%d%s)"
             (wtf--kind-label kind) repo (length items)
             (if (>= (length items) wtf-item-limit)
                 (format ", reached limit of %d" wtf-item-limit)
               ""))))

(defun wtf--show-items (kind repo args)
  "Fetch open items of KIND on REPO filtered by ARGS, then display them.
The fetch runs in the background, so Emacs stays responsive."
  (message "Fetching %s on %s..." (wtf--kind-label kind) repo)
  (wtf--gh-json-async
   (wtf--list-command kind repo args)
   (lambda (items) (wtf--render-items kind repo args items))))

(defun wtf-list-open-prs (&optional args)
  "Show open pull requests on the active repo.
ARGS are extra gh filter switches, normally supplied by
`wtf-github-dispatch'."
  (interactive (list (transient-args 'wtf-github-dispatch)))
  (wtf--show-items 'pr (wtf-active-repo) args))

(defun wtf-list-open-issues (&optional args)
  "Show open issues on the active repo.
ARGS are extra gh filter switches, normally supplied by
`wtf-github-dispatch'."
  (interactive (list (transient-args 'wtf-github-dispatch)))
  (wtf--show-items 'issue (wtf-active-repo) args))


;;;; Dispatch

(transient-define-prefix wtf-github-dispatch ()
  "Dispatch a GitHub command."
  [:description (lambda () (format "Owner: %s   Active repo: %s"
                                   (wtf-active-owner)
                                   (or wtf--active-repo "none")))
   ("-m" "Only mine"         "--author=@me")
   ("-d" "Exclude drafts"    "--draft=false")
   ("-r" "Review requested"  "--search=review-requested:@me")]
  ["Browse"
   ("o" "Set active org"   wtf-set-active-org)
   ("r" "Set active repo"  wtf-set-active-repo)
   ("p" "List open PRs"    wtf-list-open-prs)
   ("i" "List open issues" wtf-list-open-issues)]
  ["Maintenance"
   ("c" "Clear repo cache"     wtf-clear-repo-cache)
   ("k" "Kill listing buffers" wtf-kill-list-buffers)
   ("q" "Quit"                 transient-quit-one)])

;; Lives here rather than in init.el, which is tangled from init.org and
;; would lose the binding on the next tangle.  C-c g is taken by
;; `wtf-transient-gptel-prefix', so GitHub gets the shifted key.
(keymap-global-set "C-c G" #'wtf-github-dispatch)


(provide 'wtf-github)
;;; wtf-github.el ends here
