;;; wisp-smoke.el --- Batch smoke test for wisp3 -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'python)

(load-file (expand-file-name "wisp.el" default-directory))

(defun wisp3--call-backend (_request)
  (let* ((file "<buffer>")
	 (patch (format "--- %s\n+++ %s\n@@ -1,1 +1,1 @@\n-# TODO\n+pass\n"
			file file)))
    `((ok . t) (patch . ,patch))))

(let ((file (make-temp-file "wisp3-smoke-" nil ".py")))
  (with-temp-file file
    (insert "# TODO\n"))
  (with-current-buffer (find-file-noselect file)
    (python-mode)
    (setq transient-mark-mode t)
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char (point-max))
    (setq mark-active t)
    (cl-letf (((symbol-function 'wisp3--show-diff)
	       (lambda (patch)
		 (unless (> (length patch) 0)
		   (error "Empty patch")))))
      (wisp3-apply-comment))
    (kill-buffer)))

(message "wisp3 smoke test passed")

;;; wisp-smoke.el ends here
