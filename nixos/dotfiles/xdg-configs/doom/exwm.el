;;; exwm.el -*- lexical-binding: t; -*-

;; Window Manager
(require 'exwm)
(require 'exwm-config)
(exwm-config-example)

;; Resolution
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "DP-1"))

(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP-1 --mode auto --output eDP1 --off")))
(exwm-randr-enable)

;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; Launch applications with super + p
(map! "s-p"
      (lambda
        (command)
        (interactive
         (list
          (read-shell-command "$ ")))
        (start-process-shell-command command nil command)))
