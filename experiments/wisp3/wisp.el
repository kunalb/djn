;;; wisp.el --- Wisp MVP command via ACP -*- lexical-binding: t; -*-

(require 'acp)
(require 'diff-mode)
(require 'json)
(require 'subr-x)

(defgroup wisp3 nil
  "Convert comments into code patches."
  :group 'tools)

(defcustom wisp3-lamp-socket (or (getenv "LAMP_UDS") "/tmp/lamp.sock")
  "Unix socket path for the LAMP JSON-RPC server."
  :type 'string
  :group 'wisp3)

(defcustom wisp3-acp-command "nc"
  "Command used by ACP to connect to the LAMP Unix socket."
  :type 'string
  :group 'wisp3)

(defcustom wisp3-model (or (getenv "WISP_MODEL") "gpt-4o-mini")
  "Model name for OpenAI-compatible chat completions."
  :type 'string
  :group 'wisp3)

(defcustom wisp3-temperature nil
  "Optional temperature for chat completions."
  :type '(choice (const :tag "default" nil) number)
  :group 'wisp3)

(defcustom wisp3-max-tokens nil
  "Optional max tokens for chat completions."
  :type '(choice (const :tag "default" nil) integer)
  :group 'wisp3)

(defvar wisp3--client nil)
(defvar wisp3--client-socket nil)

(defun wisp3--client-live-p ()
  "Return non-nil if the cached ACP client has a live process."
  (let ((proc (alist-get :process wisp3--client)))
    (and proc (process-live-p proc))))

(defun wisp3--maybe-setup-evil ()
  "Define a few Evil bindings for diff-mode buffers when Evil is loaded."
  (when (featurep 'evil)
    (evil-define-key 'normal diff-mode-map
      (kbd "a") #'diff-apply-hunk
      (kbd "q") #'quit-window
      (kbd "gr") #'revert-buffer
      (kbd "]h") #'diff-hunk-next
      (kbd "[h") #'diff-hunk-prev
      (kbd "]c") #'diff-hunk-next
      (kbd "[c") #'diff-hunk-prev)))

(defun wisp3--language-from-mode ()
  "Best-effort language name from `major-mode`."
  (pcase major-mode
    ('python-mode "python")
    ('emacs-lisp-mode "elisp")
    (_ "unknown")))

(defun wisp3--client ()
  "Return a cached ACP client, reconnecting if needed."
  (when (or (null wisp3--client)
      (not (string= wisp3--client-socket wisp3-lamp-socket))
      (not (wisp3--client-live-p)))
    (when wisp3--client
      (ignore-errors (acp-shutdown :client wisp3--client)))
    (setq wisp3--client-socket wisp3-lamp-socket)
    (setq wisp3--client
    (acp-make-client
     :context-buffer (current-buffer)
     :command wisp3-acp-command
     :command-params (list "-U" wisp3-lamp-socket))))
  wisp3--client)

(defun wisp3--call-backend (request)
  "Send REQUEST to the LAMP server and return parsed JSON response."
(let* ((messages (wisp3--prompt-messages request))
   (messages (if (vectorp messages) messages (vconcat messages)))
   (params `((model . ,wisp3-model)
       (messages . ,messages)))
   (params (if wisp3-temperature
         (append params `((temperature . ,wisp3-temperature)))
       params))
   (params (if wisp3-max-tokens
         (append params `((max_tokens . ,wisp3-max-tokens)))
       params))
   (rpc `((:method . "chat.completions")
    (:params . ,params)))
   (result (condition-case err
         (acp-send-request :client (wisp3--client)
               :request rpc
               :sync t)
       (error
        (user-error "LAMP request failed: %s" (error-message-string err)))))
   (rpc-error (or (alist-get 'error result) (alist-get :error result)))
   (payload (or (alist-get 'result result) (alist-get :result result) result))
   (choices (alist-get 'choices payload))
   (choice (cond
      ((vectorp choices) (aref choices 0))
      ((listp choices) (car choices))
      (t nil)))
   (message (and choice (alist-get 'message choice)))
   (content (and message (alist-get 'content message))))
    (when rpc-error
      (user-error "LAMP error: %s" (or (alist-get 'message rpc-error) rpc-error)))
    (unless content
      (user-error "Missing content in chat response"))
    (wisp3--parse-backend-response content)))

(defun wisp3--show-diff (patch)
  "Show PATCH in a diff buffer."
  (let ((diff-buf (get-buffer-create "*wisp diff*")))
    (with-current-buffer diff-buf
      (erase-buffer)
      (insert patch)
      (diff-mode)
      (wisp3--maybe-setup-evil)
      (goto-char (point-min))
      (when (re-search-forward "^@@ " nil t)
  (beginning-of-line)))
    (display-buffer diff-buf)
    (select-window (get-buffer-window diff-buf))
    (message "Press `a` in the diff buffer to apply the hunk.")))

(defun wisp3--parse-backend-response (content)
  "Parse CONTENT from the model into the expected backend response."
  (let* ((clean (wisp3--strip-code-fence content))
   (parsed (json-parse-string clean
            :object-type 'alist
            :object-key-type 'symbol)))
    parsed))

(defun wisp3--strip-code-fence (content)
  "Remove markdown code fences from CONTENT if present."
  (let ((trimmed (string-trim content)))
    (if (and (string-prefix-p "```" trimmed)
       (string-suffix-p "```" trimmed))
  (string-trim
   (replace-regexp-in-string
    "\\````[a-zA-Z0-9_-]*" ""
    (replace-regexp-in-string "```\\'" "" trimmed)))
      trimmed)))

(defun wisp3--prompt-messages (request)
  "Build chat messages for REQUEST."
  (let* ((file-path (or (alist-get 'file_path request) "<buffer>"))
   (language (or (alist-get 'language request) "unknown"))
   (comment-style (or (alist-get 'comment_style request) "line"))
   (selection (alist-get 'selection request))
   (start-pos (or (alist-get 'start_pos selection) 0))
   (end-pos (or (alist-get 'end_pos selection) 0))
   (buffer-text (or (alist-get 'buffer_text request) ""))
   (selection-text (wisp3--selection-text buffer-text start-pos end-pos))
   (system
    (string-join
     '("You convert a comment region into code and return a unified diff patch."
       "Output ONLY valid JSON with keys: ok (bool), patch (string), rationale (string)."
       "Do not wrap JSON in markdown or include extra text."
       "The patch must be a unified diff for the provided file_path.")
     " "))
   (user
    (string-join
     (list
      (format "file_path: %s" file-path)
      (format "language: %s" language)
      (format "comment_style: %s" comment-style)
      (format "selection_start: %s" start-pos)
      (format "selection_end: %s" end-pos)
      "selection_text:"
      selection-text
      "buffer_text:"
      buffer-text)
     "\n")))
    (vector `((role . "system") (content . ,system))
      `((role . "user") (content . ,user)))))

(defun wisp3--selection-text (buffer-text start-pos end-pos)
  "Extract the selection substring from BUFFER-TEXT."
  (let* ((start (max 0 (1- start-pos)))
   (end (max start (1- end-pos)))
   (limit (length buffer-text))
   (end (min end limit)))
    (if (> end start)
  (substring buffer-text start end)
      "")))

;;;###autoload
(defun wisp3-apply-comment ()
  "Generate a patch from the active region and open it in a diff buffer."
  (interactive)
  (unless (use-region-p)
    (user-error "Select a comment region first"))
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let* ((start (region-beginning))
   (end (region-end))
   (request `(("file_path" . ,buffer-file-name)
        ("language" . ,(wisp3--language-from-mode))
        ("comment_style" . "line")
        ("selection" . (("start_pos" . ,start)
            ("end_pos" . ,end)))
        ("buffer_text" . ,(buffer-substring-no-properties
               (point-min)
               (point-max)))
        ("context" . (("project_root" . ,(or (vc-root-dir) ""))))))
   (response (wisp3--call-backend request)))
    (if (alist-get 'ok response)
  (wisp3--show-diff (alist-get 'patch response))
      (let* ((err (alist-get 'error response))
       (msg (alist-get 'message err)))
  (user-error "Backend error: %s" (or msg "unknown"))))))

(provide 'wisp3)

;;; wisp.el ends here
