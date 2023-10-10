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
(setq doom-font (font-spec :family "Hasklug Nerd Font" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'kaolin-ocean)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; (add-hook prog-mode-hook)
;; (setq truncate-lines nil)

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

;; (load! "./exwm.el")
(load! "./org.el")
(load! "./my-functions.el")
(load! "./keymaps.el")
(load! "./javascript.el")
(load! "./email.el")
(load! "./macros.el")

;; My status bar
;; (display-battery-mode)
;; (display-time-mode)
;; (spotify-enable-song-notifications)

;; My "screen saver"
;; (require 'zone)
;; (setq zone-programs [zone-pgm-martini-swan-dive])
;; (zone-when-idle 105)

;; Cursor style
;; (setq blink-cursor-interval 0.9)
;; (setq blink-cursor-delay 0.1)
;; (setq blink-cursor-blinks-done 0)
;; (setq blink-cursor-blinks 0)
;; (blink-cursor-mode)

;; Set my quick launch terminal
(setq terminal-here-terminal-command (list "terminal"))

;; Evil-matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; (add-to-list 'display-buffer-alist '("^\\*Flycheck errors\\*$"))

(defadvice! prompt-for-buffer (&rest _)
  :after 'window-split (switch-to-buffer))


;; Disable cursor movement when exiting insert mode
(setq evil-move-cursor-back nil)
(setq evil-respect-visual-line-mode t)

;; Authinfo (forge)
(setq auth-sources '("~/.authinfo"))

;; File associations
(push '("\\.mdx\\'" . markdown-mode) auto-mode-alist)
;(push '("\\.tsx\\'" . rjsx-mode) auto-mode-alist)
(push '("\\.js\\'" . rjsx-mode) auto-mode-alist)

;; Doom's private directory
(setq doom-user-dir "/home/juboba/repositories/personal-configuration/dotfiles/xdg-configs/doom")


;; Set branch name max length
(setq doom-modeline-vcs-max-length 40)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; Hooks
(add-hook 'after-init-hook #'global-emojify-mode)
(add-hook 'after-change-major-mode-hook #'my/doom-modeline-conditional-buffer-encoding)
;(add-hook 'prog-mode-hook 'prettify-symbols-mode)
;(add-hook 'prog-mode-hook 'my/add-pretty-lambda)
(add-hook 'prog-mode-hook 'nyan-mode)
(add-hook 'rjsx-mode-hook 'lsp)
(add-hook 'prog-mode-hook 'visual-line-mode)

;; Magit
;; (setq magit-git-global-arguments (delete "--literal-pathspecs" magit-git-global-arguments))
(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
(setq magit-diff-refine-hunk 'all)

;; Default browser
(setq browse-url-browser-function 'eww-browse-url)

;(require 'google-translate)
;(require 'google-translate-default-ui)
;(global-set-key "\C-ct" 'google-translate-at-point)
;(global-set-key "\C-cT" 'google-translate-query-translate)

;; Doom splash image
(setq fancy-splash-image (expand-file-name "emacs-e-template.svg" doom-user-dir))

(setq lsp-signature-auto-activate nil)
(setq lsp-ui-sideline-enable nil)
(setq flycheck-popup-tip-error-prefix "🛑 ")

(use-package! kubernetes)

(use-package! kubernetes-evil
  :after kubernetes)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; Jsonian
;; To enable jsonian to work with flycheck
(after! (jsonian flycheck) (jsonian-enable-flycheck))

;; To disable so-long mode overrides
(after! (jsonian so-long) (jsonian-no-so-long-mode))

(after! evil-mode
  (evil-set-register ?f
                     (kmacro-lambda-form [?O return ?u ?s ?e ?E ?f ?f ?e ?c ?t ?\( ?\( ?\) ?\S-  ?= ?> ?\S-  ?\{ return ?\} ?, ?  ?\[ ?\] escape ?k ?c ?c ?c ?o ?n ?s ?o ?l ?e ?. ?l ?o ?g ?\( ?\' ?a ?s ?t ?\' ?\) ?\; escape ?k ?^] 0 "%d")))

(use-package google-translate
  :config (setq
           google-translate-translation-directions-alist
           '(("en" . "de") ("de" . "en"))))

(use-package shell-pop
  :config (setq
           shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell))))
           shell-pop-autocd-to-working-dir nil))

(add-hook 'evil-insert-state-entry-hook #'my/use-absolute-line-numbers)
(add-hook 'evil-insert-state-exit-hook #'my/use-relative-line-numbers)

(add-hook 'prog-mode-hook (lambda () (lsp-ui-mode -1)))

(transient-define-prefix my/dispatch ()
  "Invoke something"
  ["Some commands"
   [("a" "   Find org file" my/find-file-in-org-directory)]
   [("b" "   Dragon drop" my/dragon-drop)]])

(use-package! gpt
  :config
  (setq gpt-openai-key "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"))

;;; config.el ends here
