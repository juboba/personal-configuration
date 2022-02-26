;;; org.el -*- lexical-binding: t; -*-

;; Minted (syntax highlight)
(require 'org)
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
