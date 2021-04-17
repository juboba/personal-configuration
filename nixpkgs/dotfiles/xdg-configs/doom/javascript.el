;;; javascript.el -*- lexical-binding: t; -*-

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

(add-hook 'rjsx-mode-hook 'editorconfig-mode)
(add-hook 'typescript-mode-hook 'editorconfig-mode)
