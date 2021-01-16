;;; my-functions.el -*- lexical-binding: t; -*-

(defun my/eslint-autofix ()
  "Fixes linting errors in the current buffer"
    (interactive)
    (save-buffer)
    (shell-command (concat "eslint_d" " " buffer-file-name " --fix > /dev/null; echo 'done!'"))
    (revert-buffer))

(defun my/add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ;("lambda" . 955) ; λ
          ;("->" . 8594)    ; →
          ("=>" . 955)    ; 8658 ⇒
          ("() =>" . 955)    ; 8658 ⇒
          ;; ("map" . 8614)    ; ↦
          ;; (".map" . 8614)    ; ↦
          ;; ("pipe" . 10689)    ; ↳
          ;; ("compose" . 10688)    ; ↱
          ("===" . 8801)    ; ≡
          ("!==" . 8802)    ; ≢
          ("..." . 8943)
          )))

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
  (setq tide-format-options '(:indentSize n :tabSize n)))

(defun my/set-office-code-style ()
  (interactive)
  (message "Office code style loaded!")
  (setq indent-tabs-mode t) ; use tab instead of space
  (my/setup-indent 2)) ; indent 2 spaces width

(defun my/doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(defun my/find-file-in-org-directory ()
  "Search for a file in `org-directory'."
  (interactive)
  (doom-project-find-file org-directory))

;; stolen from spacemacs :smile:
(defun spacemacs/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

;; stolen from spacemacs :smile:
(defun spacemacs/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))
