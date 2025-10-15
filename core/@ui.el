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

(use-package frame
  :unless noninteractive
  :preface
  (defvar kooky--mode-line-enabled-segments
    '((kooky--mode-line-buffer-modified . (prog-mode org-mode)))
    "Associates mode-line segments with the major modes that enable them.")

  (defun kooky--mode-line-segment (segment)
    "Returns SEGMENT if it is enabled for the current major mode, or if not restricted."
    (let ((modes (alist-get segment kooky--mode-line-enabled-segments nil nil #'eq)))
      (when (or (null modes) (apply #'derived-mode-p modes))
        (funcall segment))))

  (defun kooky--mode-line-separator (&optional width)
    "Returns a separator segment comprised of WIDTH spaces."
    (let ((padding (max 1 (or width 1))))
      (make-string padding ?\s)))

  (defun kooky--mode-line-buffer-modified ()
    "Returns the buffer-modified segment."
    (format "[%s]" (format-mode-line "%*")))

  (defun kooky--mode-line-buffer-name ()
    "Returns the buffer-name segment."
    (propertize
     (format "%s" (format-mode-line "%b"))
     'face 'mode-line-buffer-id))

  (defun kooky--mode-line-major-mode ()
    "Returns the major-mode segment."
    (propertize
     (format "%s" (format-mode-line mode-name))
     'face 'mode-line-emphasis))

  (defun kooky--mode-line-render (left right)
    "Returns a formatted string spanning `window-width'."
    (let* ((l (format-mode-line left))
           (r (format-mode-line right))
           (pad (- (window-total-width) (length l) (length r))))
      (concat l (make-string pad ?\s) r)))

  (defun kooky--mode-line ()
    "Returns the mode line format specification."
    '((:eval (kooky--mode-line-render
              (quote ((:eval (kooky--mode-line-segment 'kooky--mode-line-separator))
                      (:eval (kooky--mode-line-segment 'kooky--mode-line-buffer-name))
                      (:eval (kooky--mode-line-segment 'kooky--mode-line-separator))
                      (:eval (kooky--mode-line-segment 'kooky--mode-line-buffer-modified))))
              (quote ((:eval (kooky--mode-line-segment 'kooky--mode-line-major-mode))
                      (:eval (kooky--mode-line-segment 'kooky--mode-line-separator))))))))
  :config
  ;; Number of lines above and below the point.
  (setq scroll-margin 10)

  ;; Avoid sudden recentering during navigation.
  ;; Large values (`101+') effectively disable this completely.
  (setq scroll-conservatively 101)

  ;; Enable faster scrolling through unfontified regions.
  ;; Improves performance at the cost of inaccurate highlights.
  (setq fast-but-imprecise-scrolling t)

  ;; Preserve the point's position when paging.
  (setq scroll-preserve-screen-position t)

  ;; Disable automatic per-line vertical scrolling.
  ;; Improves performance and resolves half-jumps when scrolling.
  (setq auto-window-vscroll nil)

  ;; Prevent the cursor from blinking.
  (setq blink-cursor-mode nil)

  ;; Suppress the default save messages.
  (setq save-silently t)

  ;; Load the custom `kooky--mode-line'.
  (setq-default mode-line-format (kooky--mode-line))

(use-package window
  :unless noninteractive
  :config
  ;; Enable `window-divider-mode' after initialization.
  (add-hook 'after-init-hook #'window-divider-mode))

(use-package tab-bar
  :unless noninteractive
  :config
  ;; Enable `tab-bar-mode' after initialization.
  (add-hook 'after-init-hook #'tab-bar-mode))

(use-package fringe
  :unless noninteractive
  :config
  ;; Enable `fringe-mode' after initialization.
  (add-hook 'emacs-init-hook (lambda () (fringe-mode '(8 . 0)))))

(use-package minibuffer
  :unless noninteractive
  :config
  ;; Prefer single-key answers for `read-answer' prompts.
  (setq read-answer-short t)

  ;; Accept `y/n' instead of `yes/no' globally.
  (setq use-short-answers t)

  ;; Display active key-sequences in the minibuffer.
  ;; This is the equivalent of `showcmd' in vim.
  (setq echo-keystrokes 0.2)

  ;; Allow for recursive minibuffers during input.
  ;; This is needed by advanced commands and completion UIs.
  (setq enable-recursive-minibuffer t)

  ;; Prevent the cursor from entering the minibuffer prompt.
  ;; This makes the prompt text non-editable and immutable.
  (plist-put minibuffer-prompt-properties 'cursor-intangible t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package display-line-numbers
  :unless noninteractive
  :preface
  (defvar kooky--display-line-number-hooks
    '(prog-mode-hook text-mode-hook conf-mode-hook)
    "List of major mode hooks where `display-line-numbers-mode' is enabled.")
  :config
  ;; Prefer relative line-numbers for overall navigation.
  ;; However keep the current line absolute (vim-like).
  (setq-default display-line-numbers-type 'relative)
  (setq-default display-line-numbers-current-absolute t)

  ;; Reserve a fixed width for the line-number display.
  ;; This reduces the cost from computing the space dynamically.
  (setq-default display-line-numbers-width 3)

  ;; Enable `display-line-numbers-mode' in common editing modes.
  ;; This adds a hook for all items in `kooky--display-line-number-hooks'.
  (dolist (hook kooky--display-line-number-hooks)
    (add-hook hook #'display-line-numbers-mode)))

(use-package hl-line
  :unless noninteractive
  :preface
  (defvar kooky--display-hl-line-hooks
    '(prog-mode-hook text-mode-hook conf-mode-hook)
    "List of major mode hooks where `hl-line-mode' is enabled.")
  :config
  ;; Only highlight the current line in the selected window.
  ;; This prevents non-selected windows from tracking their own point.
  (setq hl-line-sticky-flag nil)

  ;; Enable `hl-line-mode' in common editing modes.
  ;; This adds a hook for all items in `kooky--display-hl-line-hooks'.
  (dolist (hook kooky--display-hl-line-hooks)
    (add-hook hook #'hl-line-mode)))

(use-package paren
  :unless noninteractive
  :config
  ;; Reduce the delay before matching.
  (setq show-paren-delay 0.1)

  ;; Highlight the matching pair when point is inside.
  (setq show-paren-when-point-inside-paren t)

  ;; Highlight the matching pair when point is near.
  (setq show-paren-when-point-in-periphery t)

  ;; Prevent parentheses from blinking.
  (setq blink-matching-paren nil)

  ;; Enable `show-paren-mode' after initialization.
  (add-hook 'after-init-hook #'show-paren-mode))

(use-package smartparens
  :unless noninteractive
  :config
  ;; Load the upstream defaults for common languages.
  ;; See: https://github.com/fuco1/smartparens/blob/master/smartparens-config.el
  (require 'smartparens-config)

  ;; Disable `smartparens' overlays.
  ;; This is already handled by `show-paren-mode'.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Limit prefix scanning to avoid performance hits.
  ;; This prevents slowdowns when scanning large expressions.
  (setq sp-max-prefix-length 25)

  ;; Enable `smartparens-global-mode' after initialization.
  (add-hook 'after-init-hook #'smartparens-global-mode))

(provide '@ui)

;;; @ui.el ends here
