(use-package evil
  :preface
  (defvar kooky--evil-disabled-modes
    '(vterm-mode eshell-mode)
    "List of modes where `evil-mode' is disabled.")

  (defun kooky--evil-adjust-scroll-margin ()
    "Adjust the `scroll-margin' near end-of-buffer.
This prevents extra lines at EOB when the point is within that distance."
    (unless (or (minibufferp) (window-minibuffer-p))
      (let* ((n (default-value 'scroll-margin))
             (new (if (and (> n 0) (save-excursion (forward-line n) (eobp))) 0 n)))
        (unless (eq scroll-margin new)
          (setq-local scroll-margin new)))))

  (defun kooky--evil-adjust-shift-width ()
    "Adjust `evil-shift-width' to mirror `tab-width'.
This keeps the indentation operators (>> and <<) aligned across major modes."
    (unless (derived-mode-p 'org-mode)
      (setq-local evil-shift-width tab-width)))

  (defun kooky--evil-display-save-message ()
    "Display save confirmation with file statistics.
This shows the file name, line count, and character count after saving."
    (message "%s %dL %dC written"
             (or (file-relative-name buffer-file-name) (buffer-name))
             (count-lines (point-min) (point-max))
             (buffer-size)))
  :init
  ;; Prefer `undo-fu' as the default undo-backend.
  (setq evil-undo-system 'undo-fu)

  ;; Prefer a uniform cursor shape across motions.
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  :config
  ;; Adjust `scroll-margin' near end-of-buffer.
  (add-hook 'post-command-hook #'kooky--evil-adjust-scroll-margin)

  ;; Keep `evil-shift-width' consistent with `tab-width'.
  (add-hook 'after-change-major-mode-hook #'kooky--evil-adjust-shift-width)

  ;; Display `kooky--evil-display-save-message' after saving.
  (add-hook 'after-save-hook #'kooky--evil-display-save-message)

  ;; Set the specific modes where `evil-mode' is disabled.
  (dolist (mode kooky--evil-disabled-modes)
    (evil-set-initial-state mode 'emacs))

  ;; Enable `evil-mode' after initialization.
  (add-hook 'after-init-hook #'evil-mode))

(provide '@evil)

;;; @evil.el ends here
