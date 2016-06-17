(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Vim
(ensure-package-installed 'evil 'evil-matchit 'evil-surround 'evil-tabs 'vimish-fold 
                          'powerline 'powerline-evil)

;; Magit
(ensure-package-installed 'magit 'magit-gitflow)

;; Coding helpers
(ensure-package-installed 'emmet-mode 'flycheck 'auto-complete 'js2-mode 'linum-relative
                          'rainbow-delimiters
                          'web-mode 'yasnippet 'rainbow-mode)
;; Helpers
(ensure-package-installed 'helm 'ag 'fiplr 'zoom-window)

;; Themes
(ensure-package-installed 'suscolors-theme)

;; activate installed packages
(package-initialize)
