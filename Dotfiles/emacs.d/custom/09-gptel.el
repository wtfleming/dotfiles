;; ------- gptel -------
;; Functions to include the gptel backend and model in responses from an LLM
(defun wtf-gptel-backend-and-model ()
  "Return gptel backend and model"
  (let ((backend (if (boundp 'gptel-backend) (aref gptel-backend 1)))
        (model (if (boundp 'gptel-model) gptel-model)))
    (format "(%s %s)" backend model)))

;; (defun wtf-gptel-insert-model-in-non-gptel-buffers ()
;;   "This function will add the backend and model in the \"dynamic\" buffers, not in dedicated chat buffers.
;; To be used in `gptel-pre-response-hook'."
;;   (unless (member 'gptel-mode local-minor-modes)
;;     (goto-char (point-max))
;;     (insert (format "\n%s: " (wtf-gptel-backend-and-model)))
;;     (goto-char (point-max))))

(defun wtf-gptel-insert-model-in-chat-buffers (response-begin-pos response-end-pos)
  "This function adds the backend and model in dedicated chat buffers.
Can be used with the `gptel-post-response-functions' hook."
  (let* ((gptel-org-prefix (alist-get 'org-mode gptel-prompt-prefix-alist))
         (inserted-string (format "%s %s\n"
                                  (substring gptel-org-prefix 0 (string-match " " gptel-org-prefix))
                                  (wtf-gptel-backend-and-model)))
         (len-inserted (length inserted-string )))
    (goto-char response-begin-pos)
    (insert inserted-string)
    (goto-char (+ response-end-pos len-inserted))))

;; For Ollama, You should have at least 8 GB of RAM available to run the 7B models,
;; 16 GB to run the 13B models, and 32 GB to run the 33B models.
(use-package gptel
  :ensure t
  :config
  ;; (add-hook 'gptel-pre-response-hook 'wtf-gptel-insert-model-in-non-gptel-buffers)
  (add-hook 'gptel-post-response-functions 'wtf-gptel-insert-model-in-chat-buffers)
  (gptel-make-ollama "Ollama" ; Can be any name of your choosing
    :host "localhost:11434"
    :stream t
    :models '(deepseek-r1:7b deepseek-r1:14b qwen2.5-coder:14b-instruct-q6_K gemma2 llava))

  ;; Use Claude as the default model
  ;; Fetches key from ~/.authinfo
  ;; The line should look like this:
  ;; machine api.anthropic.com login apikey password <api-key>
  (setopt
   gptel-model 'claude-3-7-sonnet-20250219
   gptel-backend (gptel-make-anthropic "Claude"
                 :stream t
                 :key gptel-api-key))

  ;; Override default system message to remove the bit about living in
  ;; emacs as sometimes an LLM gets confused and thinks questions are
  ;; about emacs when they are not
  (let ((my-gptel-system-msg "You are a large language model and a helpful assistant. Respond concisely."))
    (setopt gptel-directives (assoc-delete-all 'default gptel-directives))
    (add-to-list 'gptel-directives `(default . ,my-gptel-system-msg) )
    (setopt gptel--system-message my-gptel-system-msg)))

;; ------- gptel tools -------
(gptel-make-tool
 :name "read_buffer"                    ; snake_case name
 :function (lambda (buffer)             ; the function that will run
             (unless (buffer-live-p (get-buffer buffer))
               (error "error: buffer %s is not live." buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
               :type string             ; :type value must be a symbol
               :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")                     ; An arbitrary label for grouping



;; ------- gptel functions -------
(defun wtf-gptel-stash-response (buffer prompt response)
  "Store a response in a well known buffer we can look at if we want"
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert prompt)
      (insert "\n\n-->\n\n")
      (insert response))))

(defvar wtf-gptel-define-word-prompt
  "Please give a short definition of this word or phrase. Then, provide 3 usage examples, synonyms and antonyms"
  "The ChatGPT style prompt used to define a word.")

(defun wtf-gptel-define-word (start end)
  "Use ChatGPT to define the current word of the region."
  (interactive "r")
  (unless (region-active-p)
    (error "you must have a region set"))
  (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request nil
      :callback (lambda (response info)
                  (wtf-gptel-stash-response "*Last Definition*" (plist-get info :context) response)
                  (message response))
      :system wtf-gptel-define-word-prompt
      :context input)))
