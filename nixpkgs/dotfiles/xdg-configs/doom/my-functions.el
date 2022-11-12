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
          ;; ("() =>" . 955)    ; 8658 ⇒
          ;; ("map" . 8614)    ; ↦
          ;; (".map" . 8614)    ; ↦
          ;; ("pipe" . 10689)    ; ↳
          ;; ("compose" . 10688)    ; ↱
          ("===" . 8801)    ; ≡
          ("!==" . 8802)    ; ≢
          ("..." . 8943)
          )))

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

;; stolen from spacemacs :rofl:
(defun spacemacs/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))

(defun my/insert-coauthors ()
  "Insert the current branch's coauthors at the end of the commit message."
  (interactive)
  (insert "\n\nCo-authored by " (magit-get (concat (magit-get-current-branch) ".coauthors")))
  (goto-char 0))

(defun my/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
      (interactive "*P\nr")
      (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun home-manager-switch ()
  "Switch to current home-manager configuration."
  (interactive)
  (shell-command "home-manager switch --impure --flake ~/repositories/personal-configuration/nixos"))

(evil-set-register ?b
   (kmacro-lambda-form [?? ?> return ?l ?v ?/ ?\; return ?s ?\{ escape ?w ?i return ?r ?e ?t ?u ?r ?n ?  escape ?$ ?i return escape ?A ?\; escape ?k] 0 "%d"))

(defun home-manager-edit ()
  (interactive)
  (doom-project-find-file "~/repositories/personal-configuration"))
