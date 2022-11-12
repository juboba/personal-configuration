;;; keymaps.el -*- lexical-binding: t; -*-

;; My Mappings
(map! :leader "\"" 'terminal-here-launch)

(map! :leader
      (:prefix-map ("j" . "juboba")
        :desc "pause music" "p" 'spotify-playpause
        :desc "pretty symbols" "t" 'prettify-symbols-mode
        :desc "emojis" "e" 'emojify-mode
        :desc "Highlight indentation" "i" 'highlight-indentation-mode
        :desc "Copy link at point" :mode mu4e-view-mode "l" 'link-hint-copy-link-at-point

       (:prefix ("h" . "Home Manager")
        :desc "open file" "e" #'home-manager-edit
        :desc "reload" "s" #'home-manager-switch)))

(map! :leader :prefix "t"
      "i" 'imenu-list-smart-toggle
      :desc "Toggle aufo-fill" "f" 'auto-fill-mode
      :desc "Toggle minimap" "m" 'minimap-mode)

(map! :leader :desc "Comment lines" "c l" 'evilnc-comment-or-uncomment-lines)
(map! :leader :desc "Find Org file" "o o" 'my/find-file-in-org-directory)

;; Flycheck
(map! :leader :prefix ("e" . "errors")
      :desc "previous error" "N" 'flycheck-previous-error
      :desc "next error" "n" 'flycheck-next-error
      :desc "open error list" "l" 'spacemacs/toggle-flycheck-error-list
      :desc "open and focus error list" "L" 'spacemacs/goto-flycheck-error-list
      :desc "fix all errors (lsp)" "f" 'lsp-eslint-apply-all-fixes)

;; Buffers and windows:
(map! :leader "b D" 'kill-buffer-and-window)

(map! :leader :prefix "w"
      "D" 'ace-delete-window
      "W" 'ace-swap-window
      "w" 'ace-window
      :desc "only " "o" 'doom/window-maximize-buffer
      :desc "maximize" "m" 'doom/window-enlargen
      "f" 'projectile-find-file-other-window
      "F" 'find-file-other-window)

;; Magit
(map! :leader :prefix "g"
      :desc "Magit status" "s" 'magit-status
      :desc "Magit blame" "b" 'magit-blame-addition
      :desc "Magit switch to branch" "B" 'magit-branch-checkout)

;; Evil
(define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
