;;; keymaps.el -*- lexical-binding: t; -*-

;; My Mappings
(map! :leader "\"" 'terminal-here-launch)
(map! :leader :prefix ("j" . "juboba") :desc "pause music" "p" 'spotify-playpause)
(map! :leader :desc "pretty symbols" "j t" 'prettify-symbols-mode)
(map! :leader :desc "emojis" "j e" 'emojify-mode)
(map! :leader :desc "go to definition" "j d" 'evil-goto-definition)
(map! :leader "t i" 'imenu-list-smart-toggle)
(map! :leader :desc "Comment lines" "c l" 'evilnc-comment-or-uncomment-lines)
(map! :leader :desc "Find Org file" "o o" 'my/find-file-in-org-directory)

;; Flycheck
(map! :leader :prefix ("e" "errors") "N" 'flycheck-previous-error)
(map! :leader "e n" 'flycheck-next-error)
(map! :leader "e l" 'spacemacs/toggle-flycheck-error-list)
(map! :leader "e L" 'spacemacs/goto-flycheck-error-list)
(map! :leader "e f" 'my/eslint-autofix)

;; Buffers and windows:
(map! :leader "b D" 'kill-buffer-and-window)
(map! :leader "w D" 'ace-delete-window)

;; Magit
(map! :leader :desc "Magit status" "g s" 'magit-status)
(map! :leader :desc "Magit blame" "g b" 'magit-blame-addition)
(map! :leader :desc "Magit switch to branch" "g B" 'magit-branch-checkout)

(define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
