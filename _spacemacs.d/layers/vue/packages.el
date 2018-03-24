;;; package --- Summary
;;; Commentary:
;;; This layer provides vue-mode support
;;; Code:
(setq vue-packages
      '(
        vue-mode
        )
      )

(setq vue-excluded-packages '())

(defun vue/init-vue-mode ()
  "Initialize my package."
  (use-package vue-mode
    :config
    ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
    (setq mmm-submode-decoration-level 0)))

;;; packages.el ends here
