;;; email.el -*- lexical-binding: t; -*-

;; Email config
;(setq mu4e-root-maildir "~/Maildir/genially")

(set-email-account! "genial.ly"
                    '((mu4e-drafts-folder     . "/[Gmail].Borradores")
                      (mu4e-sent-folder       . "/[Gmail].Enviados")
                      (mu4e-trash-folder      . "/[Gmail].Papelera")
                      (smtpmail-smtp-user     . "juboba@genial.ly")
                      (mu4e-compose-signature . "
*Julio Borja Barra* · *Developer*

[[~/Pictures/genially.png]]"))
                    t)

;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(setq +mu4e-backend 'offlineimap)

;; Set get mail command when mu4e starts
(add-hook
 'mu4e-main-mode-hook
 (lambda ()
   (setq mu4e-get-mail-command "offlineimap -o")))

;; Interesting mail to consider as 'unread' for alerts
(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " AND maildir:"
       "\"/INBOX\""))

;; Use mail notifications
(mu4e-alert-set-default-style 'libnotify)

;; Initial Hooks
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
