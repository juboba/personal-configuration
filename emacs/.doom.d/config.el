;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Julio Borja Barra"
      user-mail-address "juboba@genially.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-horizon)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Org/Dropbox/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (load-file "~/.doom.d/exwm.el")
(load-file "~/.doom.d/org.el")
(load-file "~/.doom.d/my-functions.el")
(load-file "~/.doom.d/keymaps.el")
(load-file "~/.doom.d/javascript.el")
(load-file "~/.doom.d/email.el")

;; Set Undo
;; (setq evil-undo-system 'undo-tree)
;; (setq evil-undo-function 'undo-tree-undo)
;; (setq evil-redo-function 'undo-tree-redo)

;; My status bar
;; (display-battery-mode)
;; (display-time-mode)
(spotify-enable-song-notifications)

;; My "screen saver"
(require 'zone)
(setq zone-programs [zone-pgm-rat-race])
(zone-when-idle 105)

;; Cursor style
;; (setq blink-cursor-interval 0.2)
;; (setq blink-cursor-delay 0.1)
;; (setq blink-cursor-blinks-done 0)
;; (blink-cursor-mode)

;; Set my quick launch terminal
(setq terminal-here-terminal-command (list "/home/juboba/.bin/terminal"))

;; Set indentation
(my/set-office-code-style)

;; Evil-matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; (add-to-list 'display-buffer-alist '("^\\*Flycheck errors\\*$"))
(defadvice! prompt-for-buffer (&rest _)
  :after 'window-split (switch-to-buffer))

;; Set default font
(add-to-list 'default-frame-alist '(font . "Hasklug Nerd Font 13"))
;; For quickly setting the fonr use:
;; (set-frame-font "Hasklug Nerd Font 13" nil t)


;; Disable cursor movement when exiting insert mode
(setq evil-move-cursor-back nil)

;; Hooks
(add-hook 'after-init-hook #'global-emojify-mode)
(add-hook 'after-change-major-mode-hook #'my/doom-modeline-conditional-buffer-encoding)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'my/add-pretty-lambda)

(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
