;;; package --- Org configuration
;;; org.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; Minted (syntax highlight)
(require 'org)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Org/")
(setq org-agenda-files '("~/Documents/Org/Dropbox"))
(setq org-roam-directory "~/Documents/Org/Dropbox")

(after! org
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; Org-roam
(setq org-roam-graph-filetype "pdf")

(use-package! websocket
    :after org-roam)

;; Revealjs root
(setq org-reveal-root "file:///home/juboba/.local/reveal.js")

(require 'ox-reveal)
(require 'ox-hugo)

;; Github Flavored Markdown export in Org
(eval-after-load "org"
  '(require 'ox-gfm nil t))

(add-hook 'org-mode-hook 'auto-fill-mode)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
;;; org.el ends here
