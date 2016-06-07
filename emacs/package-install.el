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

(ensure-package-installed 'evil 'evil-matchit 'evil-surround 'evil-tabs
                          'magit 'magit-gitflow
                          'emmet-mode 'flycheck 'fiplr
                          'helm
                          'js2-mode 'linum-relative 'powerline 'rainbow-delimiters
                          'vimish-fold 'web-mode 'yasnippet 'ag)

;; activate installed packages
(package-initialize)
