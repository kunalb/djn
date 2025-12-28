;;; wisp-ui-demo.el --- UI demos for patch apply flows -*- lexical-binding: t; -*-

(require 'diff-mode)
(require 'ediff)

(defun wisp-ui-demo--maybe-setup-evil ()
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

(defgroup wisp-ui-demo nil
  "Demo commands for comparing patch apply UIs."
  :group 'tools)

(defconst wisp-ui-demo--sample-text
  "def compute():\n    # TODO: implement\n    return 0\n"
  "Sample file contents for UI demos.")

(defun wisp-ui-demo--make-diff (file)
  "Return a unified diff string that patches FILE."
  (format
   (concat
    "--- %s\n"
    "+++ %s\n"
    "@@ -1,3 +1,3 @@\n"
    " def compute():\n"
    "-    # TODO: implement\n"
    "-    return 0\n"
    "+    value = 1 + 1\n"
    "+    return value\n")
   file
   file))

(defun wisp-ui-demo--write-temp-file ()
  "Create a temp file with sample content and return its path."
  (let ((file (make-temp-file "wisp-ui-demo-" nil ".py")))
    (with-temp-file file
      (insert wisp-ui-demo--sample-text))
    file))

;;;###autoload
(defun wisp-ui-demo-hunk-apply ()
  "Open a diff buffer and let the user apply a single hunk."
  (interactive)
  (let* ((file (wisp-ui-demo--write-temp-file))
         (diff (wisp-ui-demo--make-diff file))
         (diff-buf (get-buffer-create "*wisp-ui-demo diff*"))
         (file-buf (find-file-noselect file)))
    (with-current-buffer diff-buf
      (erase-buffer)
      (insert diff)
      (diff-mode)
      (wisp-ui-demo--maybe-setup-evil)
      (goto-char (point-min))
      (when (re-search-forward "^@@ " nil t)
        (beginning-of-line)))
    (display-buffer file-buf)
    (display-buffer diff-buf)
    (select-window (get-buffer-window diff-buf))
    (message "Press `a` in the diff buffer to apply the hunk (diff-apply-hunk). Temp file: %s" file)))

;;;###autoload
(defun wisp-ui-demo-ediff-patch ()
  "Launch ediff-patch on a temp file using a generated patch."
  (interactive)
  (let* ((file (wisp-ui-demo--write-temp-file))
         (patch-file (make-temp-file "wisp-ui-demo-" nil ".patch")))
    (with-temp-file patch-file
      (insert (wisp-ui-demo--make-diff file)))
    (message "Ediff patch demo. Temp file: %s" file)
    (ediff-patch-file file patch-file)))

(provide 'wisp-ui-demo)

;;; wisp-ui-demo.el ends here
