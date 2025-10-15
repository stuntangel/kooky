;;
;; Copyright (C) 2025 Ryan Niemi
;;
;; Author: Ryan Niemi 
;; URL: https://github.com/stuntangel/kooky
;;
;;
;;; Commentary:
;;
;;; Code:

(use-package vterm
  :unless noninteractive
  :commands (vterm vterm-other-window)
  :config
  ;; Kill the buffer immediately when the terminal exits.
  ;; This prevents the accumulation of dead terminal buffers.
  (setq vterm-kill-buffer-on-exit t)

  ;; Reduce the delay before refreshing the buffer for updates.
  ;; This provides *way* more responsive output at higher CPU cost.
  (setq vterm-timer-delay 0.01))

(use-package xclip
  :unless (display-graphic-p)
  :config
  ;; Enable `xclip-mode' in TTY sessions.
  ;; This allows yanking to system clipboard and pasting from it.
  (add-hook 'tty-setup-hook #'xclip-mode))

(provide '@tty)

;;; @tty.el ends here
