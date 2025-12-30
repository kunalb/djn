;;; wisp.el --- Wisp MVP command -*- lexical-binding: t; -*-

(require 'diff-mode)
(require 'json)
(require 'subr-x)

(defgroup wisp nil
  "Convert comments into code patches."
  :group 'tools)

(defcustom wisp-lamp-socket (or (getenv "LAMP_UDS") "/tmp/lamp.sock")
  "Unix socket path for the LAMP JSON-RPC server."
  :type 'string
  :group 'wisp)

(defcustom wisp-model (or (getenv "WISP_MODEL") "gpt-4o-mini")
  "Model name for OpenAI-compatible chat completions."
  :type 'string
  :group 'wisp)

(defcustom wisp-temperature nil
  "Optional temperature for chat completions."
  :type '(choice (const :tag "default" nil) number)
  :group 'wisp)

(defcustom wisp-max-tokens nil
  "Optional max tokens for chat completions."
  :type '(choice (const :tag "default" nil) integer)
  :group 'wisp)

(defcustom wisp-request-timeout 20
  "Seconds to wait for a response from the LAMP server."
  :type 'integer
  :group 'wisp)

(defun wisp--maybe-setup-evil ()
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

(defun wisp--language-from-mode ()
  "Best-effort language name from `major-mode`."
  (pcase major-mode
    ('python-mode "python")
    ('emacs-lisp-mode "elisp")
    (_ "unknown")))

(defun wisp--call-backend (request)
  "Send REQUEST to the LAMP server and return parsed JSON response."
  (let* ((messages (wisp--prompt-messages request))
	 (params `(("model" . ,wisp-model)
		   ("messages" . ,messages)))
	 (params (if wisp-temperature
		     (append params `(("temperature" . ,wisp-temperature)))
		   params))
	 (params (if wisp-max-tokens
		     (append params `(("max_tokens" . ,wisp-max-tokens)))
		   params))
	 (rpc `(("jsonrpc" . "2.0")
		("id" . 1)
		("method" . "chat.completions")
		("params" . ,params)))
	 (raw (wisp--lamp-request (json-encode rpc)))
	 (response (json-parse-string raw
				      :object-type 'alist
				      :array-type 'list
				      :object-key-type 'symbol))
	 (rpc-error (alist-get 'error response))
	 (result (alist-get 'result response))
	 (choices (alist-get 'choices result))
	 (message (alist-get 'message (car choices)))
	 (content (alist-get 'content message)))
    (when rpc-error
      (user-error "LAMP error: %s" (alist-get 'message rpc-error)))
    (unless content
      (user-error "Missing content in chat response"))
    (wisp--parse-backend-response content)))

(defun wisp--lamp-request (payload)
  "Send PAYLOAD to the LAMP socket and return raw response text."
  (let* ((buf (get-buffer-create "*wisp-lamp-output*"))
	 (proc (make-network-process :name "wisp-lamp"
				     :buffer buf
				     :family 'local
				     :service wisp-lamp-socket
				     :coding 'utf-8
				     :noquery t)))
    (with-current-buffer buf
      (erase-buffer))
    (process-send-string proc (concat payload "\n"))
    (wisp--wait-for-response proc buf)
    (let ((text (with-current-buffer buf
		  (buffer-substring-no-properties (point-min) (point-max)))))
      (delete-process proc)
      (string-trim text))))

(defun wisp--wait-for-response (proc buf)
  "Wait for PROC to write a newline into BUF or timeout."
  (let ((deadline (+ (float-time) wisp-request-timeout)))
    (while (and (process-live-p proc)
		(not (with-current-buffer buf
		       (goto-char (point-min))
		       (search-forward "\n" nil t)))
		(< (float-time) deadline))
      (accept-process-output proc 0.05))
    (unless (with-current-buffer buf
	      (goto-char (point-min))
	      (search-forward "\n" nil t))
      (delete-process proc)
      (user-error "Timed out waiting for LAMP response"))))

(defun wisp--parse-backend-response (content)
  "Parse CONTENT from the model into the expected backend response."
  (let* ((clean (wisp--strip-code-fence content))
	 (parsed (json-parse-string clean
				    :object-type 'alist
				    :object-key-type 'symbol)))
    parsed))

(defun wisp--strip-code-fence (content)
  "Remove markdown code fences from CONTENT if present."
  (let ((trimmed (string-trim content)))
    (if (and (string-prefix-p "```" trimmed)
	     (string-suffix-p "```" trimmed))
	(string-trim
	 (replace-regexp-in-string
	  "\\````[a-zA-Z0-9_-]*" ""
	  (replace-regexp-in-string "```\\'" "" trimmed)))
      trimmed)))

(defun wisp--prompt-messages (request)
  "Build chat messages for REQUEST."
  (let* ((file-path (or (alist-get 'file_path request) "<buffer>"))
	 (language (or (alist-get 'language request) "unknown"))
	 (comment-style (or (alist-get 'comment_style request) "line"))
	 (selection (alist-get 'selection request))
	 (start-pos (or (alist-get 'start_pos selection) 0))
	 (end-pos (or (alist-get 'end_pos selection) 0))
	 (buffer-text (or (alist-get 'buffer_text request) ""))
	 (selection-text (wisp--selection-text buffer-text start-pos end-pos))
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
    (list `(("role" . "system") ("content" . ,system))
	  `(("role" . "user") ("content" . ,user)))))

(defun wisp--selection-text (buffer-text start-pos end-pos)
  "Extract the selection substring from BUFFER-TEXT."
  (let* ((start (max 0 (1- start-pos)))
	 (end (max start (1- end-pos)))
	 (limit (length buffer-text))
	 (end (min end limit)))
    (if (> end start)
	(substring buffer-text start end)
      "")))

(defun wisp--show-diff (patch)
  "Show PATCH in a diff buffer."
  (let ((diff-buf (get-buffer-create "*wisp diff*")))
    (with-current-buffer diff-buf
      (erase-buffer)
      (insert patch)
      (diff-mode)
      (wisp--maybe-setup-evil)
      (goto-char (point-min))
      (when (re-search-forward "^@@ " nil t)
	(beginning-of-line)))
    (display-buffer diff-buf)
    (select-window (get-buffer-window diff-buf))
    (message "Press `a` in the diff buffer to apply the hunk.")))

;;;###autoload
(defun wisp-apply-comment ()
  "Generate a patch from the active region and open it in a diff buffer."
  (interactive)
  (unless (use-region-p)
    (user-error "Select a comment region first"))
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let* ((start (region-beginning))
	 (end (region-end))
	 (request `(("file_path" . ,buffer-file-name)
		    ("language" . ,(wisp--language-from-mode))
		    ("comment_style" . "line")
		    ("selection" . (("start_pos" . ,start)
				    ("end_pos" . ,end)))
		    ("buffer_text" . ,(buffer-substring-no-properties
				       (point-min)
				       (point-max)))
		    ("context" . (("project_root" . ,(or (vc-root-dir) ""))))))
	 (response (wisp--call-backend request)))
    (if (alist-get 'ok response)
	(wisp--show-diff (alist-get 'patch response))
      (let* ((err (alist-get 'error response))
	     (msg (alist-get 'message err)))
	(user-error "Backend error: %s" (or msg "unknown"))))))

(provide 'wisp)

;;; wisp.el ends here
