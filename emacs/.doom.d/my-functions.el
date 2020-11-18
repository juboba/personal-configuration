;;; my-functions.el -*- lexical-binding: t; -*-

(defun my/eslint-autofix ()
  "Fixes linting errors in the current buffer"
    (interactive)
    (shell-command (concat "eslint_d" " " buffer-file-name " --fix > /dev/null; echo 'done!'"))
    )

(defun my/add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ;("lambda" . 955) ; λ
          ;("->" . 8594)    ; →
          ("=>" . 955)    ; 8658 ⇒
          ("() =>" . 955)    ; 8658 ⇒
          ("map" . 8614)    ; ↦
          (".map" . 8614)    ; ↦
          ("pipe" . 10689)    ; ↳
          ("compose" . 10688)    ; ↱
          ("===" . 8801)    ; ≡
          ("!==" . 8802)    ; ≢
          ("..." . 8943)
          )))
