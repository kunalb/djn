;;; wisp.el --- Wisp MVP command -*- lexical-binding: t; -*-

(require 'diff-mode)
(require 'json)

(defgroup wisp nil
  "Convert comments into code patches."
  :group 'tools)

(defcustom wisp-backend-command '("python3" "-m" "wisp_backend")
  "Command to invoke the Wisp backend."
  :type '(repeat string)
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
  "Send REQUEST to backend and return parsed JSON response."
  (let* ((input (json-encode request))
         (program (car wisp-backend-command))
         (args (cdr wisp-backend-command))
         (output-buf (get-buffer-create "*wisp-backend-output*")))
    (with-current-buffer output-buf
      (erase-buffer))
    (with-temp-buffer
      (insert input)
      (let ((exit-code (apply #'call-process-region
                              (point-min)
                              (point-max)
                              program
                              nil
                              output-buf
                              nil
                              args)))
        (with-current-buffer output-buf
          (goto-char (point-min))
          (let ((json-object-type 'alist))
            (if (zerop exit-code)
                (json-parse-string (buffer-substring-no-properties (point-min) (point-max))
                                   :object-type 'alist)
              (json-parse-string (buffer-substring-no-properties (point-min) (point-max))
                                 :object-type 'alist))))))))

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
