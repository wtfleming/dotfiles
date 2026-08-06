;;; wtf-docker.el --- Code to interact with Docker  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Will Fleming

;; Author: Will Fleming <wfleming77@gmail.com>
;; Keywords: tools
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.2") (transient "0.9.0"))

;;; Commentary:

;; Manage Docker from Emacs by shelling out to the `docker' command
;; line tool.
;;
;; `C-c d' opens a transient dispatch with listings for containers,
;; images, builds, volumes, and compose projects.  Each listing is a
;; `vtable'.  RET inspects the item at point, w copies its identifier,
;; S sorts by the column at point, g refetches, and q closes the
;; listing.  Containers also have s (start), o (stop), r (restart),
;; D (remove), l (follow logs), and e (open a shell); images and
;; volumes have D (remove); compose projects have u (up), d (down),
;; r (restart), and l (follow logs).
;;
;; The compose transient (`C-c d C') resolves the compose file above
;; the current buffer's directory, so a project that has never been
;; started can still be brought up.  Compose output streams into a
;; buffer as the command runs.
;;
;; Listings are fetched fresh every time, in the background, so Emacs
;; stays responsive.  Mutations (stop, remove, up, ...) also run in
;; the background and refetch the affected listing when they finish.

;;; Code:
(require 'ansi-color)
(require 'map)
(require 'term)
(require 'transient)
(require 'vtable)


;;;; Customization

(defgroup wtf-docker nil
  "Manage Docker using the `docker' command line tool."
  :group 'tools
  :prefix "wtf-docker-")

(defcustom wtf-docker-log-tail 200
  "Number of existing log lines to show when following logs."
  :type 'natnum
  :group 'wtf-docker)

(defcustom wtf-docker-shell-command
  "command -v bash >/dev/null && exec bash || exec sh"
  "Command run inside a container to start an interactive shell.
Prefers bash when the image has it, falling back to sh."
  :type 'string
  :group 'wtf-docker)


;;;; Running docker

(defun wtf-docker--program ()
  "Return the `docker' executable.
Signal an error when it is missing from the variable `exec-path', which
differs from the shell PATH when Emacs is started from a GUI."
  (or (executable-find "docker")
      (user-error "Cannot find docker; check the variable `exec-path'")))

(defun wtf-docker--run (&rest args)
  "Run docker with ARGS and return its output as a string.
Signal a `user-error' carrying docker's own message when it fails,
rather than letting the caller trip over unparseable output."
  (with-temp-buffer
    (let ((status (apply #'call-process (wtf-docker--program) nil t nil args)))
      (unless (zerop status)
        (user-error "Command docker %s failed: %s" (string-join args " ")
                    (string-trim (buffer-string))))
      (buffer-string))))

(defun wtf-docker--parse-ndjson (output args)
  "Parse OUTPUT as newline-delimited JSON, returning a list of objects.
Most docker listings emit one JSON object per line rather than an
array.  Blame the docker invocation ARGS on failure."
  (condition-case nil
      (mapcar (lambda (line) (json-parse-string line :null-object nil))
              (split-string output "\n" t))
    (json-parse-error
     (user-error "Command docker %s returned unexpected output: %s"
                 (string-join args " ") (string-trim output)))))

(defun wtf-docker--parse-json-array (output args)
  "Parse OUTPUT as a single JSON array, returning a list of objects.
Only `compose ls' emits an array; the other listings are
newline-delimited.  Blame the docker invocation ARGS on failure."
  (condition-case nil
      (append (json-parse-string output :null-object nil) nil)
    (json-parse-error
     (user-error "Command docker %s returned unexpected output: %s"
                 (string-join args " ") (string-trim output)))))

(defun wtf-docker--json-async (args parser callback)
  "Run docker with ARGS in the background, calling CALLBACK with parsed items.
PARSER turns docker's raw output into a list of objects.  Failures are
reported in the echo area and CALLBACK is not called."
  (let ((buf (generate-new-buffer " *wtf-docker*")))
    (make-process
     :name "wtf-docker"
     :buffer buf
     :noquery t
     ;; A pipe, not the default pty: on a tty docker switches to
     ;; human-formatted output, which the JSON parsers reject.
     :connection-type 'pipe
     :command (cons (wtf-docker--program) args)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((status (process-exit-status proc))
               (output (with-current-buffer buf (buffer-string))))
           (kill-buffer buf)
           (if (not (zerop status))
               (message "Command docker %s failed: %s"
                        (string-join args " ") (string-trim output))
             ;; The parser signals `user-error', which must not escape
             ;; a sentinel; report it in the echo area instead.
             (condition-case err
                 (funcall callback (funcall parser output args))
               (user-error (message "%s" (error-message-string err)))))))))))

(defun wtf-docker--run-then-refresh (kind description &rest args)
  "Run docker with ARGS in the background, then refetch the KIND listing.
DESCRIPTION is echoed while the command runs and when it finishes.
The listing is only refetched when its buffer still exists.  Mutations
go through here because they can be slow: `docker stop' alone takes ten
seconds when the container ignores SIGTERM."
  (let ((buf (generate-new-buffer " *wtf-docker*")))
    (message "%s..." description)
    (make-process
     :name "wtf-docker"
     :buffer buf
     :noquery t
     :connection-type 'pipe
     :command (cons (wtf-docker--program) args)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((status (process-exit-status proc))
               (output (with-current-buffer buf (buffer-string))))
           (kill-buffer buf)
           (if (not (zerop status))
               (message "%s failed: %s" description (string-trim output))
             (message "%s...done" description)
             (when (get-buffer (wtf-docker--kind-buffer kind))
               (wtf-docker--show-items kind)))))))))


;;;; Presentation

(defface wtf-docker-list-row '((t))
  "Face for ordinary rows in a listing.
Deliberately empty; it is the unstriped half of `:row-colors'."
  :group 'wtf-docker)

(defface wtf-docker-list-row-alt
  '((((background light)) :background "#f2f2f2")
    (((background dark))  :background "#2b2b2b"))
  "Face for alternating rows in a listing.
Sets only a background, so the state column keeps its own foreground."
  :group 'wtf-docker)

(defun wtf-docker--format-age (seconds)
  "Format SECONDS, a Unix timestamp, as a compact age such as \"3h\"."
  (let ((age (- (float-time) seconds)))
    (cond ((< age 3600)  (format "%dm" (truncate (/ age 60))))
          ((< age 86400) (format "%dh" (truncate (/ age 3600))))
          (t             (format "%dd" (truncate (/ age 86400)))))))

;; A timestamp rather than a formatted string: vtable sorts on the
;; getter's value, so "10m" would sort before "3h" alphabetically.
(defun wtf-docker--parse-time (time)
  "Return TIME as a Unix timestamp.
Container and image timestamps look like \"2026-06-11 17:06:54 -0700
PDT\", whose trailing zone name `date-to-time' rejects, so it is
dropped.  Buildx timestamps are ISO 8601 and pass through."
  (let ((parts (split-string time " ")))
    (float-time (date-to-time (if (> (length parts) 3)
                                  (string-join (take 3 parts) " ")
                                time)))))

(defun wtf-docker--container-state (item)
  "Return container ITEM's state as a short coloured string.
A running container with a failing health check still reports state
\"running\"; the health verdict only appears in the status text, hence
the separate unhealthy test."
  (let* ((state (map-elt item "State"))
         (status (or (map-elt item "Status") ""))
         (face (pcase state
                 ("running" (if (string-search "unhealthy" status)
                                'error
                              'success))
                 ((or "exited" "created") 'shadow)
                 ((or "paused" "restarting") 'warning)
                 ("dead" 'error))))
    (if face (propertize state 'face face) state)))

(defun wtf-docker--build-status (status)
  "Return build STATUS as a short coloured string."
  (propertize status 'face (pcase status
                             ("Completed" 'success)
                             ("Error" 'error)
                             (_ 'warning))))

(defun wtf-docker--compose-status (status)
  "Return compose project STATUS, like \"running(2)\", coloured."
  (propertize status 'face (cond ((string-prefix-p "running" status) 'success)
                                 ((string-prefix-p "exited" status) 'shadow)
                                 (t 'warning))))

(defun wtf-docker--build-steps (item)
  "Summarize build ITEM's steps as \"done/total (cached)\"."
  (format "%s/%s (%s cached)"
          (map-elt item "completed_steps")
          (map-elt item "total_steps")
          (map-elt item "cached_steps")))

(defun wtf-docker--short-ref (ref)
  "Return the last component of build REF.
`buildx history ls' reports refs like \"desktop-linux/desktop-linux/x7…\"
but the inspect and logs subcommands only accept the final component."
  (car (last (split-string ref "/"))))

(defun wtf-docker--image-name (item)
  "Return image ITEM's repository:tag, or its ID when untagged."
  (let ((repo (map-elt item "Repository")))
    (if (equal repo "<none>")
        (map-elt item "ID")
      (format "%s:%s" repo (map-elt item "Tag")))))


;;;; Kinds

(defun wtf-docker--container-getter (item column _table)
  "Return container ITEM's value for COLUMN."
  (pcase column
    (0 (map-elt item "Names"))
    (1 (wtf-docker--container-state item))
    (2 (map-elt item "Status"))
    (3 (map-elt item "Image"))
    (4 (map-elt item "Ports"))
    (5 (wtf-docker--parse-time (map-elt item "CreatedAt")))))

(defun wtf-docker--image-getter (item column _table)
  "Return image ITEM's value for COLUMN."
  (pcase column
    (0 (map-elt item "Repository"))
    (1 (map-elt item "Tag"))
    (2 (map-elt item "ID"))
    (3 (map-elt item "Size"))
    (4 (wtf-docker--parse-time (map-elt item "CreatedAt")))))

(defun wtf-docker--build-getter (item column _table)
  "Return build ITEM's value for COLUMN."
  (pcase column
    (0 (map-elt item "name"))
    (1 (wtf-docker--build-status (map-elt item "status")))
    (2 (wtf-docker--build-steps item))
    (3 (wtf-docker--parse-time (map-elt item "created_at")))
    (4 (wtf-docker--short-ref (map-elt item "ref")))))

(defun wtf-docker--volume-getter (item column _table)
  "Return volume ITEM's value for COLUMN."
  (pcase column
    (0 (map-elt item "Name"))
    (1 (map-elt item "Driver"))
    (2 (map-elt item "Scope"))
    (3 (map-elt item "Mountpoint"))))

(defun wtf-docker--compose-getter (item column _table)
  "Return compose project ITEM's value for COLUMN."
  (pcase column
    (0 (map-elt item "Name"))
    (1 (wtf-docker--compose-status (map-elt item "Status")))
    (2 (map-elt item "ConfigFiles"))))

(defconst wtf-docker--kinds
  '((container
     :label "containers"
     :command ("ps" "-a" "--format" "{{json .}}")
     :parse wtf-docker--parse-ndjson
     :id-key "ID"
     :getter wtf-docker--container-getter
     :columns ((:name "Name")
               (:name "State")
               (:name "Status")
               (:name "Image")
               (:name "Ports")
               (:name "Created" :align right :formatter wtf-docker--format-age)))
    (image
     :label "images"
     :command ("images" "--format" "{{json .}}")
     :parse wtf-docker--parse-ndjson
     :id-key "ID"
     :getter wtf-docker--image-getter
     :columns ((:name "Repository")
               (:name "Tag")
               (:name "ID")
               (:name "Size" :align right)
               (:name "Created" :align right :formatter wtf-docker--format-age)))
    (build
     :label "builds"
     :command ("buildx" "history" "ls" "--format" "json")
     :parse wtf-docker--parse-ndjson
     :id-key "ref"
     :getter wtf-docker--build-getter
     :columns ((:name "Name")
               (:name "Status")
               (:name "Steps" :align right)
               (:name "Created" :align right :formatter wtf-docker--format-age)
               (:name "Ref")))
    (volume
     :label "volumes"
     :command ("volume" "ls" "--format" "json")
     :parse wtf-docker--parse-ndjson
     :id-key "Name"
     :getter wtf-docker--volume-getter
     :columns ((:name "Name")
               (:name "Driver")
               (:name "Scope")
               (:name "Mountpoint")))
    (compose
     :label "compose projects"
     :command ("compose" "ls" "-a" "--format" "json")
     :parse wtf-docker--parse-json-array
     :id-key "Name"
     :getter wtf-docker--compose-getter
     :columns ((:name "Name")
               (:name "Status")
               (:name "Config files"))))
  "Specification of each listing: command, parser, columns, and getter.
`:id-key' names the field that identifies an item, used to keep point
on the same row across a refetch.")

(defun wtf-docker--kind-get (kind prop)
  "Return PROP from the `wtf-docker--kinds' entry for KIND."
  (plist-get (alist-get kind wtf-docker--kinds) prop))

(defun wtf-docker--kind-buffer (kind)
  "Return the listing buffer name for KIND."
  (format "*wtf-docker-%s*" (wtf-docker--kind-get kind :label)))


;;;; Listings

(defun wtf-docker-list-quit ()
  "Kill the current buffer and restore the previous window layout.
Killing the buffer also kills any process attached to it."
  (interactive)
  (quit-window t))

(defun wtf-docker-kill-list-buffers ()
  "Kill every wtf-docker listing, inspect, log, and shell buffer."
  (interactive)
  (let ((n 0))
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*wtf-docker-" (buffer-name buf))
        (kill-buffer buf)
        (setq n (1+ n))))
    (message "Killed %d wtf-docker buffer%s" n (if (= n 1) "" "s"))))

(defun wtf-docker--goto-item (kind id)
  "Move point to the KIND row whose identifier is ID.
Return non-nil when a matching row was found."
  (let ((key (wtf-docker--kind-get kind :id-key)))
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (when (equal id (map-elt (vtable-current-object) key))
          (throw 'found t))
        (forward-line 1))
      nil)))

(defun wtf-docker--list-actions (kind)
  "Return the vtable :actions list for KIND."
  (append
   ;; `vtable-map' binds g to `vtable-revert-command', which only
   ;; redraws the cached objects.  Its keymap is a text property on
   ;; every row, so it shadows `special-mode's g; override it here.
   (list "g" (lambda (_item) (wtf-docker--show-items kind))
         "w" (lambda (item)
               (let ((id (map-elt item (wtf-docker--kind-get kind :id-key))))
                 (kill-new id)
                 (message "Copied %s" id))))
   (pcase kind
     ('container
      (list
       "RET" (lambda (item)
               (wtf-docker--inspect (map-elt item "Names")
                                    "inspect" (map-elt item "ID")))
       "s" (lambda (item)
             (wtf-docker--run-then-refresh
              'container (format "Starting %s" (map-elt item "Names"))
              "start" (map-elt item "ID")))
       "o" (lambda (item)
             (wtf-docker--run-then-refresh
              'container (format "Stopping %s" (map-elt item "Names"))
              "stop" (map-elt item "ID")))
       "r" (lambda (item)
             (wtf-docker--run-then-refresh
              'container (format "Restarting %s" (map-elt item "Names"))
              "restart" (map-elt item "ID")))
       "D" (lambda (item)
             (let ((name (map-elt item "Names")))
               (when (y-or-n-p (format "Remove container %s? " name))
                 (wtf-docker--run-then-refresh
                  'container (format "Removing %s" name)
                  "rm" (map-elt item "ID")))))
       "l" (lambda (item)
             (wtf-docker--follow (map-elt item "Names")
                                 "logs" "-f"
                                 "--tail" (number-to-string wtf-docker-log-tail)
                                 (map-elt item "ID")))
       "e" (lambda (item)
             (wtf-docker--shell (map-elt item "Names") (map-elt item "ID")))))
     ('image
      (list
       "RET" (lambda (item)
               (wtf-docker--inspect (wtf-docker--image-name item)
                                    "inspect" (map-elt item "ID")))
       ;; Remove by repository:tag, not ID: removing a multiply-tagged
       ;; image by ID fails, whereas removing a tag just untags it.
       "D" (lambda (item)
             (let ((name (wtf-docker--image-name item)))
               (when (y-or-n-p (format "Remove image %s? " name))
                 (wtf-docker--run-then-refresh
                  'image (format "Removing %s" name)
                  "rmi" name))))))
     ('build
      (list
       "RET" (lambda (item)
               (let ((ref (wtf-docker--short-ref (map-elt item "ref"))))
                 (wtf-docker--inspect ref "buildx" "history" "inspect" ref)))
       "l" (lambda (item)
             (let ((ref (wtf-docker--short-ref (map-elt item "ref"))))
               (wtf-docker--follow ref "buildx" "history" "logs" ref)))))
     ('volume
      (list
       "RET" (lambda (item)
               (wtf-docker--inspect (map-elt item "Name")
                                    "volume" "inspect" (map-elt item "Name")))
       "D" (lambda (item)
             (let ((name (map-elt item "Name")))
               (when (y-or-n-p (format "Remove volume %s? " name))
                 (wtf-docker--run-then-refresh
                  'volume (format "Removing %s" name)
                  "volume" "rm" name))))))
     ('compose
      (list
       "RET" (lambda (item)
               (find-file (car (split-string (map-elt item "ConfigFiles") ","))))
       "u" (lambda (item)
             (wtf-docker--compose-run (map-elt item "Name")
                                      (wtf-docker--compose-file-args item)
                                      "up" "-d"))
       "d" (lambda (item)
             (let ((name (map-elt item "Name")))
               (when (y-or-n-p (format "Bring project %s down? " name))
                 (wtf-docker--compose-run name
                                          (wtf-docker--compose-file-args item)
                                          "down"))))
       "r" (lambda (item)
             (wtf-docker--compose-run (map-elt item "Name")
                                      (wtf-docker--compose-file-args item)
                                      "restart"))
       "l" (lambda (item)
             (apply #'wtf-docker--follow (map-elt item "Name")
                    (append '("compose") (wtf-docker--compose-file-args item)
                            (list "logs" "-f" "--tail"
                                  (number-to-string wtf-docker-log-tail))))))))))

(defun wtf-docker--render-items (kind items)
  "Render ITEMS of KIND into its listing buffer."
  (let* ((buf (get-buffer-create (wtf-docker--kind-buffer kind)))
         (key (wtf-docker--kind-get kind :id-key))
         ;; Remember the item under point, not the line: a refetch can
         ;; add or drop rows, and restoring by line would silently land
         ;; on a different item.  Keep the sort order too, so g does not
         ;; undo S.
         (prev (with-current-buffer buf
                 (map-elt (vtable-current-object) key)))
         (sort-by (with-current-buffer buf
                    (save-excursion
                      (goto-char (point-min))
                      (when-let ((table (vtable-current-table)))
                        (vtable-sort-by table))))))
    (with-current-buffer buf
      (special-mode)
      ;; Compose rather than mutate: `special-mode-map' is shared.
      (use-local-map (make-composed-keymap
                      (define-keymap "q" #'wtf-docker-list-quit)
                      special-mode-map))
      (setq-local revert-buffer-function
                  (lambda (&rest _) (wtf-docker--show-items kind)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (zerop (length items))
            (insert (format "No %s.\n" (wtf-docker--kind-get kind :label)))
          (make-vtable
           :columns (wtf-docker--kind-get kind :columns)
           :objects (append items nil)
           :row-colors '(wtf-docker-list-row wtf-docker-list-row-alt)
           :sort-by sort-by
           :actions (wtf-docker--list-actions kind)
           :getter (wtf-docker--kind-get kind :getter))))
      (unless (and prev (wtf-docker--goto-item kind prev))
        (goto-char (point-min))))
    ;; Outside `with-current-buffer': its `save-current-buffer' would
    ;; otherwise undo the selection when the form exits.
    (pop-to-buffer buf)
    (message "Fetching %s...done (%d)"
             (wtf-docker--kind-get kind :label) (length items))))

(defun wtf-docker--show-items (kind)
  "Fetch items of KIND and display them.
The fetch runs in the background, so Emacs stays responsive."
  (message "Fetching %s..." (wtf-docker--kind-get kind :label))
  (wtf-docker--json-async
   (wtf-docker--kind-get kind :command)
   (wtf-docker--kind-get kind :parse)
   (lambda (items) (wtf-docker--render-items kind items))))

(defun wtf-docker-list-containers ()
  "Show all containers, including stopped ones."
  (interactive)
  (wtf-docker--show-items 'container))

(defun wtf-docker-list-images ()
  "Show images."
  (interactive)
  (wtf-docker--show-items 'image))

(defun wtf-docker-list-builds ()
  "Show buildx build records."
  (interactive)
  (wtf-docker--show-items 'build))

(defun wtf-docker-list-volumes ()
  "Show volumes."
  (interactive)
  (wtf-docker--show-items 'volume))

(defun wtf-docker-list-compose-projects ()
  "Show compose projects, including stopped ones."
  (interactive)
  (wtf-docker--show-items 'compose))


;;;; Inspect, logs, and shell buffers

(defun wtf-docker--inspect (name &rest args)
  "Show the output of docker ARGS in a read-only buffer named after NAME."
  (let ((buf (get-buffer-create (format "*wtf-docker-inspect: %s*" name)))
        (output (apply #'wtf-docker--run args)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output))
      ;; `inspect' emits JSON but `buildx history inspect' and
      ;; `system df' emit plain text; pick the mode by looking.
      (if (memq (char-after (point-min)) '(?\[ ?{))
          (js-json-mode)
        (fundamental-mode))
      (setq buffer-read-only t)
      (use-local-map (make-composed-keymap
                      (define-keymap "q" #'wtf-docker-list-quit)
                      (current-local-map)))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun wtf-docker--follow-filter (proc output)
  "Insert OUTPUT at PROC's mark, rendering ANSI colour codes.
Point follows the output only when it was already at the end, so
scrolling back is not disturbed."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (at-end (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (ansi-color-apply output))
          (set-marker (process-mark proc) (point)))
        (when at-end (goto-char (process-mark proc)))))))

(defun wtf-docker--follow (name &rest args)
  "Stream the output of docker ARGS into a buffer named after NAME.
An existing process for NAME is killed first.  q kills the buffer,
which also kills the process."
  (let ((buf (get-buffer-create (format "*wtf-docker-logs: %s*" name))))
    (with-current-buffer buf
      (when-let ((proc (get-buffer-process buf)))
        (delete-process proc))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (special-mode)
      (use-local-map (make-composed-keymap
                      (define-keymap "q" #'wtf-docker-list-quit)
                      special-mode-map))
      (make-process
       :name (format "wtf-docker-logs: %s" name)
       :buffer buf
       :noquery t
       :connection-type 'pipe
       :command (cons (wtf-docker--program) args)
       :filter #'wtf-docker--follow-filter
       ;; Killing the process (q, or a fresh l) signals it, so a plain
       ;; exit is the only case worth reporting.
       :sentinel (lambda (proc _event)
                   (when (eq (process-status proc) 'exit)
                     (message "docker output for %s ended" name)))))
    (pop-to-buffer buf)))

(defun wtf-docker--shell (name id)
  "Open an interactive shell in container ID in a buffer named after NAME.
Runs `wtf-docker-shell-command' inside the container via a `term-mode'
buffer, since docker exec -it needs a pty.  Exit the shell (or C-c C-k
then kill) to end it; C-c C-j switches to line mode."
  (let ((buf (make-term (format "wtf-docker-shell: %s" name)
                        (wtf-docker--program) nil
                        "exec" "-it" id
                        "sh" "-c" wtf-docker-shell-command)))
    (with-current-buffer buf
      (term-mode)
      (term-char-mode))
    (pop-to-buffer buf)))


;;;; Compose

(defvar wtf-docker--compose-file-names
  '("docker-compose.yml" "docker-compose.yaml" "compose.yaml" "compose.yml")
  "Conventional compose file names, in the order docker itself tries them.")

(defun wtf-docker--compose-file ()
  "Return the compose file governing `default-directory', or nil.
Every directory upward is checked for all of the conventional names
before ascending, so a nearby compose.yaml beats a distant
docker-compose.yml."
  (let (file)
    (locate-dominating-file
     default-directory
     (lambda (dir)
       (setq file (seq-find #'file-exists-p
                            (mapcar (lambda (name) (expand-file-name name dir))
                                    wtf-docker--compose-file-names)))))
    file))

(defun wtf-docker--compose-context ()
  "Return (LABEL . FILE-ARGS) for the compose file above `default-directory'.
LABEL is the containing directory's name, which is also docker's
default project name.  Signal a `user-error' when no compose file is
found."
  (let ((file (wtf-docker--compose-file)))
    (unless file
      (user-error "No compose file found above %s"
                  (abbreviate-file-name default-directory)))
    (cons (file-name-nondirectory
           (directory-file-name (file-name-directory file)))
          (list "-f" file))))

(defun wtf-docker--compose-file-args (item)
  "Return -f arguments selecting compose project ITEM's file(s)."
  (mapcan (lambda (file) (list "-f" file))
          (split-string (map-elt item "ConfigFiles") ",")))

(defun wtf-docker--compose-run (label file-args &rest compose-args)
  "Run docker compose COMPOSE-ARGS on the project labelled LABEL.
FILE-ARGS select the compose file(s).  Output streams into a buffer
while the command runs; the compose listing is refetched when it
finishes, if its buffer still exists."
  (let ((buf (get-buffer-create (format "*wtf-docker-compose: %s*" label))))
    (with-current-buffer buf
      (when-let ((proc (get-buffer-process buf)))
        (delete-process proc))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (special-mode)
      (use-local-map (make-composed-keymap
                      (define-keymap "q" #'wtf-docker-list-quit)
                      special-mode-map))
      (make-process
       :name (format "wtf-docker-compose: %s" label)
       :buffer buf
       :noquery t
       :connection-type 'pipe
       :command (append (list (wtf-docker--program) "compose")
                        file-args compose-args)
       :filter #'wtf-docker--follow-filter
       :sentinel
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (message "docker compose %s on %s: %s"
                    (string-join compose-args " ") label
                    (if (zerop (process-exit-status proc)) "done" "failed"))
           (when (get-buffer (wtf-docker--kind-buffer 'compose))
             (wtf-docker--show-items 'compose))))))
    ;; Progress output is secondary; show it without stealing the window.
    (display-buffer buf)))

(defun wtf-docker-compose-up ()
  "Run compose up -d on the current buffer's project."
  (interactive)
  (pcase-let ((`(,label . ,file-args) (wtf-docker--compose-context)))
    (wtf-docker--compose-run label file-args "up" "-d")))

(defun wtf-docker-compose-down ()
  "Run compose down on the current buffer's project."
  (interactive)
  (pcase-let ((`(,label . ,file-args) (wtf-docker--compose-context)))
    (when (y-or-n-p (format "Bring project %s down? " label))
      (wtf-docker--compose-run label file-args "down"))))

(defun wtf-docker-compose-restart ()
  "Run compose restart on the current buffer's project."
  (interactive)
  (pcase-let ((`(,label . ,file-args) (wtf-docker--compose-context)))
    (wtf-docker--compose-run label file-args "restart")))

(defun wtf-docker-compose-build ()
  "Run compose build on the current buffer's project."
  (interactive)
  (pcase-let ((`(,label . ,file-args) (wtf-docker--compose-context)))
    (wtf-docker--compose-run label file-args "build")))

(defun wtf-docker-compose-logs ()
  "Follow compose logs for the current buffer's project."
  (interactive)
  (pcase-let ((`(,label . ,file-args) (wtf-docker--compose-context)))
    (apply #'wtf-docker--follow label
           (append '("compose") file-args
                   (list "logs" "-f" "--tail"
                         (number-to-string wtf-docker-log-tail))))))

(transient-define-prefix wtf-docker-compose-dispatch ()
  "Dispatch a docker compose command for the current buffer's project."
  [:description
   ;; The transient renders in its own buffer; resolve the compose file
   ;; against the buffer the transient was invoked from.
   (lambda () (format "Compose file: %s"
                      (or (with-current-buffer transient--original-buffer
                            (wtf-docker--compose-file))
                          "none found")))
   ("u" "Up (detached)" wtf-docker-compose-up)
   ("d" "Down"          wtf-docker-compose-down)
   ("r" "Restart"       wtf-docker-compose-restart)
   ("b" "Build"         wtf-docker-compose-build)
   ("l" "Logs"          wtf-docker-compose-logs)]
  ["Other"
   ("p" "List compose projects" wtf-docker-list-compose-projects)
   ("q" "Quit" transient-quit-one)])


;;;; System

(defun wtf-docker-system-df ()
  "Show docker disk usage."
  (interactive)
  (wtf-docker--inspect "system df" "system" "df"))

(defun wtf-docker-system-prune ()
  "Prune stopped containers, unused networks, dangling images, build cache."
  (interactive)
  (when (y-or-n-p "Prune stopped containers, unused networks, dangling images, and build cache? ")
    (wtf-docker--run-then-refresh 'container "Pruning" "system" "prune" "-f")))


;;;; Dispatch

(transient-define-prefix wtf-docker-dispatch ()
  "Dispatch a Docker command."
  ["Views"
   ("c" "Containers"       wtf-docker-list-containers)
   ("i" "Images"           wtf-docker-list-images)
   ("b" "Builds"           wtf-docker-list-builds)
   ("v" "Volumes"          wtf-docker-list-volumes)
   ("p" "Compose projects" wtf-docker-list-compose-projects)]
  ["Compose"
   ("C" "Compose in this project" wtf-docker-compose-dispatch)]
  ["System"
   ("d" "Disk usage" wtf-docker-system-df)
   ("P" "Prune"      wtf-docker-system-prune)]
  ["Maintenance"
   ("k" "Kill wtf-docker buffers" wtf-docker-kill-list-buffers)
   ("q" "Quit"                    transient-quit-one)])

;; Lives here rather than in init.el, which is tangled from init.org and
;; would lose the binding on the next tangle.
(keymap-global-set "C-c d" #'wtf-docker-dispatch)


(provide 'wtf-docker)
;;; wtf-docker.el ends here
