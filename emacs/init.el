;; Package repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; prevent silly initial splash screen
(setq inhibit-splash-screen t)

(cd "~/Work/altima")

;; Load theme
(load-theme 'suscolors)

;; Set default font
(when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco")
    (set-face-attribute 'default nil :height 110))

;; Load configs
(load-file "~/.emacs.d/package-install.el")
(load-file "~/.emacs.d/functions.el")
(load-file "~/.emacs.d/evil-config.el")
(load-file "~/.emacs.d/mu4e-config.el")
(load-file "~/.emacs.d/erc-config.el")

;; Helm mode
(require 'helm)
(helm-mode t)

;; Ag Silver Searcher
(require 'ag)

;; fiplr fuzzy find
(require 'fiplr)

(setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules" "bower_components" "jspm_packages" "-build"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))


;; Relative line numbers:
(add-hook 'prog-mode-hook 'linum-relative-mode t)
(add-hook 'web-mode-hook 'linum-relative-mode t)

;; Flymake-jshint for javascript sources
;(require 'flymake-jshint)
;(require 'flymake-cursor)
;(add-hook 'js-mode-hook 'flymake-mode)
;(add-hook 'js-mode-hook 'flymake-jshint-load)

;(require 'flymake-phpcs)
;(add-hook 'php-mode-hook 'flymake-phpcs-load)
(require 'flycheck)
(global-flycheck-mode)
(setq js2-mode-show-strict-warnings nil)

;; Yasnippet
;(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20150415.244/")
(require 'yasnippet)
(yas-global-mode)

;;;; KEYS

(global-set-key (kbd "C-x f") 'fiplr-find-file)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-y") 'yas-expand-from-trigger-key)
(global-set-key (kbd "C-j") 'emmet-expand-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x z") 'zoom-window-zoom)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c s") 'split-with-file)
(global-set-key (kbd "C-c v") 'vsplit-with-file)
(global-set-key (kbd "C-c f") 'comint-dynamic-complete-filename)

;; Show me the matching parenthesis
(show-paren-mode t)

;; Auto-insert closing parens
(electric-pair-mode t)

;; Remove the awful toolbar, menubar and scrollbars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; iBuffer
(require 'ibuffer) 

(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Altima"
                (filename . "Work/altima"))
            ("Helm"
                (predicate string-match "Hmm" mode-name))
            ("ERC"
                (mode . erc-mode))
            ("Programming"
                (or
                    (mode . c-mode)
                    (mode . js2-mode)
                    (mode . web-mode)
                    (mode . css-mode)
                    (mode . python-mode)
                    (mode . emacs-lisp-mode)
                    ;; etc
                    ))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;; Spaces, no tabs
(setq juboba-tab-width 4)

(add-hook 'web-mode-hook
          (function (lambda ()
                      (setq evil-shift-width juboba-tab-width))))

(setq-default indent-tabs-mode nil)
(setq tab-width juboba-tab-width)
(setq js2-basic-offset juboba-tab-width)
(setq html-indent-level juboba-tab-width)
(setq css-indent-level juboba-tab-width)

(setq web-mode-markup-indent-offset juboba-tab-width)
(setq web-mode-css-indent-offset juboba-tab-width)
(setq web-mode-code-indent-offset juboba-tab-width)

(setq jade-indent-level juboba-tab-width)
(add-hook 'jade-mode-hook
  (function (lambda ()
          (setq evil-shift-width jade-indent-level))))

;; Indent new lines
(define-key global-map (kbd "RET") 'newline-and-indent)

;; js2-mode (Enhanced javascript mode)
(require 'js2-mode)

;; Web-mode
(require 'web-mode)

;; File associations
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

;; Throw backup files to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; BSD C style
;(setq-default c-basic-offset 2 c-default-style "bsd")


;; Smooth scrolling
(setq scroll-margin 0
scroll-conservatively 9999
scroll-step 1)

;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Magit shortcut
(require 'magit)

;; Magit go fullscreen
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Magit gitflow plugin
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Set default encoding
(set-language-environment "UTF-8")

;; Start the emacs server!
(setq server-use-tcp t)
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("97d039a52cfb190f4fd677f02f7d03cf7dbd353e08ac8a0cb991223b135ac4e6" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd" "335ad769bcd7949d372879ec10105245255beec6e62213213835651e2eb0b8e0" "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "cc2f32f5ee19cbd7c139fc821ec653804fcab5fcbf140723752156dc23cdb89f" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "cd2a93d7b63aff07b3565c1c95e461cb880f0b00d8dd6cdd10fa8ece01ffcfdf" "3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" "62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "49ad7c8d458074db7392f8b8a49235496e9228eb2fa6d3ca3a7aa9d23454efc6" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(global-auto-complete-mode t)
 '(web-mode-indent-style 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))
