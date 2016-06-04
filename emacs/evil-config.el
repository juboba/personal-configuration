;; Evil mode
(require 'evil)
(evil-mode 1)

(global-evil-tabs-mode t)

(global-evil-matchit-mode t)

;; Kill buffer with :q
(evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)

;; Vimish fold
(require 'vimish-fold)
(define-key evil-normal-state-map "z" nil)
(define-key evil-motion-state-map "zf" 'vimish-fold)
(define-key evil-motion-state-map "zt" 'vimish-fold-toggle)
(define-key evil-motion-state-map "zd" 'vimish-fold-delete)

;; Powerline
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)

;; Cursor color
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))
