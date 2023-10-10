;;; keymaps.el -*- lexical-binding: t; -*-

;;; Code:
(map! :leader "\"" 'terminal-here-launch)
(map! :leader "!" 'shell-pop)

(map! "C-SPC" 'execute-extended-command)

(map! :leader
      (:prefix ("j" . "juboba")
        :desc "pause music" "p" 'spotify-playpause
        :desc "pretty symbols" "s" 'prettify-symbols-mode
        :desc "emojis" "e" 'emojify-mode
        :desc "Highlight indentation" "i" 'highlight-indentation-mode
        :desc "Copy link at point" :mode mu4e-view-mode "l" 'link-hint-copy-link-at-point
        :desc "Show info" "v" 'lsp-signature-activate

        (:prefix ("c" . "conflict")
        :desc "keep upper" "u" 'smerge-keep-upper
        :desc "keep lower" "l" 'smerge-keep-lower
        :desc "keep base" "b" 'smerge-keep-base
        :desc "keep both" "a" 'smerge-keep-all
        :desc "next" "n" 'smerge-vc-next-conflict)

        (:prefix ("t" . "translate")
        :desc "translate" "t" 'google-translate-smooth-translate
        :desc "to kill ring" "y" 'translate-to-kill-ring
        :desc "replace with translation" "r" 'translate-and-replace)

        (:prefix ("r" . "remark")
        :desc "mark" "m" 'org-remark-mark
        :desc "mark yellow" "y" 'org-remark-mark-yellow
        :desc "mark red" "r" 'org-remark-mark-red-line
        :desc "view" "v" 'org-remark-view
        :desc "delete" "d" 'org-remark-delete)

        (:prefix ("h" . "Home Manager")
         :desc "edit" "e" #'home-manager-edit
         :desc "switch" "s" #'nixos-rebuild-switch)))

(map! :leader :prefix "c"
      :desc "Jump to def other window" "D" 'go-to-definition-other-window
      :desc "Jump to references" "u" '+lsp-lookup-references-handler
      :desc "Jump to ref other window" "U" 'go-to-references-other-window)

(map! :leader :prefix "f"
      :desc "Open drag-n-drop" "d" 'my/dragon-drop)

(map! :leader
      :desc "Next Workwspace" "TAB <right>" '+workspace/switch-right
      :desc "Previous Workspace" "TAB <left>" '+workspace/switch-left)

(map! :leader :prefix "t"
      "i" 'imenu-list-smart-toggle
      :desc "Toggle aufo-fill" "f" 'auto-fill-mode
      :desc "Toggle breadcrumbs" "p" 'lsp-headerline-breadcrumb-mode
      :desc "Zen mode (fullscreen)" "z" '+zen/toggle-fullscreen
      "Z" nil)

(map! :leader :desc "Comment lines" "c l" 'evilnc-comment-or-uncomment-lines)
(map! :leader :desc "Find Org file" "o o" 'my/find-file-in-org-directory)

;; Flycheck
(map! :leader :prefix ("e" . "errors")
      :desc "previous error" "N" 'flycheck-previous-error
      :desc "next error" "n" 'flycheck-next-error
      :desc "open error list" "l" 'spacemacs/toggle-flycheck-error-list
      :desc "open and focus error list" "L" 'spacemacs/goto-flycheck-error-list
      :desc "fix & save" "e" 'fix-and-save
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
      :desc "Magit blame" "b" 'magit-blame-addition
      :desc "Magit switch to branch" "B" 'magit-branch-checkout
      :desc "Magit process buffer" "p" 'magit-process-buffer
      :desc "Magit process kill" "k" 'magit-process-kill)

;; Evil
(define-key evil-visual-state-map (kbd "s") 'evil-surround-region)

(windmove-default-keybindings 'control)

;(define-key vertico-map (kbd "C-a") 'embark-act)

;;; keymaps.el ends here
