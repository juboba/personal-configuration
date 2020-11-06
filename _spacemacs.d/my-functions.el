;;;; my-functions -- My custom functions
;;;; Commentary:
;;;; This is a collection of functions that I use in my init.el.
;;;; Code:

;; Indentation from
;; http://blog.binchen.org/posts/easy-indentation-setup-in-emacs-for-web-development.html
(defun my/setup-indent (n)
  "Set the indentation for programming modes according to N."
  (setq coffee-tab-width n) ; coffeescript
  (setq react-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode
  (setq typescript-indent-level n) ; typescript-mode
  ;; (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  ;; (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  (setq tide-format-options '(:indentSize n :tabSize n))
  )

(defun my/add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
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

(defun my/has-eslint (directory)
  "Return non-nil if DIRECTORY/.bin has an eslint and is executable."
  (and (file-accessible-directory-p directory)
       (file-executable-p (concat directory "/.bin/eslint"))))
  ;; (and (s-contains-p "node_modules" directory) (file-executable-p (concat directory "/.bin/eslint")))

(defun my/parent-directory (dir)
  "Get the parent directory of DIR."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun my/find-file-with (pred current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy
that meets PRED, starting from CURRENT-DIR."
  (let ((file (concat current-dir fname))
        (parent (my/parent-directory (expand-file-name current-dir))))
    (if (funcall pred file)
        file
      (when parent
        (my/find-file-with pred parent fname)))))

(defun my/get-local-eslint ()
  "Find the eslint executable in the node_modules."
  (let (
        (node_modules-dir
         (my/find-file-with 'my/has-eslint
                            (file-name-directory buffer-file-name)
                            "node_modules")))
    (when node_modules-dir
      (format "%s/.bin/eslint" node_modules-dir))))
;;; my-functions.el ends here
