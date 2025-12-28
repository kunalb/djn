;;; wisp-smoke.el --- Batch smoke test for wisp.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'python)

(load-file (expand-file-name "wisp.el" default-directory))

(setq wisp-backend-command
      (list
       "python3"
       "-c"
       (concat
        "import json,sys; "
        "req=json.load(sys.stdin); "
        "fp=req.get('file_path','<buffer>'); "
        "patch='--- %s\\n+++ %s\\n@@ -1,1 +1,1 @@\\n-# TODO\\n+pass\\n' % (fp,fp); "
        "sys.stdout.write(json.dumps({'ok':True,'patch':patch}))")))

(let ((file (make-temp-file "wisp-smoke-" nil ".py")))
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
                 (unless (> (length patch) 0)
                   (error "Empty patch")))))
      (wisp-apply-comment))
    (kill-buffer)))

(message "wisp smoke test passed")

;;; wisp-smoke.el ends here
