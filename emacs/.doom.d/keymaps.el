;;; keymaps.el -*- lexical-binding: t; -*-

;; My Mappings
(map! :leader "\"" 'terminal-here-launch)
(map! :leader :prefix ("j" . "juboba") :desc "pause music" "p" 'spotify-playpause)
(map! :leader :desc "pretty symbols" "j t" 'prettify-symbols-mode)
(map! :leader :desc "emojis" "j e" 'emojify-mode)
(map! :leader "t i" 'imenu-list-smart-toggle)

;; Flycheck
(map! :leader "e N" 'flycheck-previous-error)
(map! :leader "e n" 'flycheck-next-error)
(map! :leader "e l" 'flycheck-list-errors)

(map! :leader "b D" 'kill-buffer-and-window)
(map! :leader :desc "Magit status" "g s" 'magit-status)

;; Workspace quick switch
(map! :leader "TAB TAB" '+workspace/other)
(map! :leader "TAB `" '+workspace/display)

(define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
