;;; javascript.el -*- lexical-binding: t; -*-

;; Use typescript-mode for tsx files
(push '("\\.tsx\\'" . typescript-mode) auto-mode-alist)
(push '("\\.js\\'" . rjsx-mode) auto-mode-alist)

(add-hook 'typescript-mode-hook 'prettify-symbols-mode)
(add-hook 'typescript-mode-hook 'my/add-pretty-lambda)

;; Use tide
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-mode)
            (tide-setup)))

  ;; Make eslint run after tide
(with-eval-after-load "tide" (flycheck-add-next-checker 'typescript-tide 'javascript-eslint))

;; Set eslint executable
(add-hook 'flycheck-mode-hook
          '(lambda ()
            (setq-local flycheck-javascript-eslint-executable "eslint_d")))
