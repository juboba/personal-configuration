;;;; my-functions -- My custom functions
;;;; Commentary:
;;;; This is a collection of functions that I use in my init.el.
;;;; Code:
(defun my/has-eslint (directory)
  "Return non-nil if DIRECTORY/.bin has an eslint and is executable."
  (and (file-accessible-directory-p directory) (file-executable-p (concat directory "/.bin/eslint"))))
  ;; (and (s-contains-p "node_modules" directory) (file-executable-p (concat directory "/.bin/eslint")))

(defun my/parent-directory (dir)
  "Get the parent directory of DIR."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun my/find-file-with (pred current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy that meets PRED, starting from CURRENT-DIR."
  (let ((file (concat current-dir fname))
        (parent (my/parent-directory (expand-file-name current-dir))))
    (if (funcall pred file)
        file
      (when parent
        (my/find-file-with pred parent fname)))))

(defun my/get-local-eslint ()
  "Find the eslint executable in the node_modules."
  (let ((node_modules-dir (my/find-file-with 'my/has-eslint (file-name-directory buffer-file-name) "node_modules")))
    (when node_modules-dir
      (concat node_modules-dir "/.bin/eslint"))))
;;; my-functions.el ends here
