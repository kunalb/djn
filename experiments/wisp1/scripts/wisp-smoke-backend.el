;;; wisp-smoke-backend.el --- Batch smoke test for real backend -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'python)

(load-file (expand-file-name "wisp.el" default-directory))

(setq wisp-backend-command '("python3" "-m" "wisp_backend"))

(let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
       (repo-root (file-name-directory (directory-file-name script-dir)))
       (file (make-temp-file (expand-file-name "wisp-smoke-" repo-root) nil ".py")))
  (with-temp-file file
    (insert "# TODO\n"))
  (with-current-buffer (find-file-noselect file)
    (python-mode)
    (setq transient-mark-mode t)
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char (point-max))
    (setq mark-active t)
    (cl-letf (((symbol-function 'wisp--show-diff)
               (lambda (patch)
                 (unless (and (> (length patch) 0)
                              (string-match-p "pass" patch))
                   (error "Unexpected patch: %s" patch)))))
      (wisp-apply-comment))
    (kill-buffer)))

(message "wisp backend smoke test passed")

;;; wisp-smoke-backend.el ends here
