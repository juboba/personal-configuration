;;; package --- Summary


;;; Commentary:

;;; Code:
(defun split-with-file (orientation file)
  "Create a new split.  Vertical if ORIENTATION is 2 (horizontal in any other case) with the selected FILE."
  (interactive "p\nfSelect file: ")
  (if (= orientation 2)
    (evil-window-vsplit nil file)
    (evil-window-split nil file)))

(defun vsplit-with-file (file)
  "Create a new vertical split with the selected FILE."
  (interactive "fSelect file: ")
  (split-with-file 2 file))
(provide 'functions)
;;; functions.el ends here
