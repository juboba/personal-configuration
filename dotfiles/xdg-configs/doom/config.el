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
;; (load! "./email.el")
(load! "./macros.el")

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

(setq evil-v$-excludes-newline t)

;; Authinfo (forge)
(setq auth-sources '("~/.authinfo"))

;; Golden ratio
(setq zoom-size '(0.618 0.618))

;; File associations
(push '("\\.mdx\\'" . markdown-mode) auto-mode-alist)
(push '("\\.js\\'" . jtsx-jsx-mode) auto-mode-alist)
(push '("\\.jsx\\'" . jtsx-jsx-mode) auto-mode-alist)
(push '("\\.yuck\\'" . lisp-mode) auto-mode-alist)
(push '("\\.ts\\'" . jtsx-typescript-mode) auto-mode-alist)
(push '("\\.tsx\\'" . jtsx-tsx-mode) auto-mode-alist)

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
;; (add-hook 'rjsx-mode-hook 'lsp)
;; (add-hook 'prog-mode-hook 'visual-line-mode)
;; (add-hook 'prog-mode-hook 'indent-bars-mode)
(add-hook 'typescript-ts-mode-hook #'lsp)
(add-hook 'js-ts-mode-hook #'lsp)
(add-hook 'jtsx-tsx-mode-hook #'lsp)

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
(setq indent-bars-width-frac 0.1)

;; LSP Performance
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

;; (use-package! tree-sitter
;;  :config
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Jsonian
;; To enable jsonian to work with flycheck
(after! (jsonian flycheck) (jsonian-enable-flycheck))

;; To disable so-long mode overrides
(after! (jsonian so-long) (jsonian-no-so-long-mode))

(after! evil-mode
  (evil-set-register ?f
                     (kmacro-lambda-form [?O return ?u ?s ?e ?E ?f ?f ?e ?c ?t ?\( ?\( ?\) ?\S-  ?= ?> ?\S-  ?\{ return ?\} ?, ?  ?\[ ?\] escape ?k ?c ?c ?c ?o ?n ?s ?o ?l ?e ?. ?l ?o ?g ?\( ?\' ?a ?s ?t ?\' ?\) ?\; escape ?k ?^] 0 "%d")))

;; (use-package google-translate
;;  :config (setq
;;           google-translate-translation-directions-alist
;;           '(("en" . "de") ("de" . "en"))))

(use-package shell-pop
  :config (setq
           shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell))))
           shell-pop-autocd-to-working-dir nil))

(add-hook 'evil-insert-state-entry-hook #'my/use-absolute-line-numbers)
(add-hook 'evil-insert-state-exit-hook #'my/use-relative-line-numbers)

(add-hook 'prog-mode-hook (lambda () (lsp-ui-mode -1)))

(transient-define-prefix my/yarn-dispatch ()
  "Invoke something"
  ["Yarn dispatch"
   [("y" "   Install" my/yarn-install)]
   [("b" "   Build deps" my/yarn-build-deps)]])

(add-to-list 'default-frame-alist '(alpha-background . 95))
;; (set-frame-parameter nil 'alpha-background 95)


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(setq lsp-enable-file-watchers nil)

(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;;; config.el ends here
