;;; ui.el --- Font configuration and management -*- lexical-binding: t; -*-

;; Author: stuntangel
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; - Ready Player music player

;;; Code:

(use-package standard-keys-mode
  ;; Enable standard-keys-mode after initializing the Emacs session.
  :hook after-init
  ;; Customize options:
  :custom
  ;; Make the C-x and C-c bindings are properly overriden (this is optional)
  (standard-keys-override-new-C-x-and-C-c-commands t)

  ;; Tell standard-keys-create-new-buffer to create a new scratch buffer
  (standard-keys-new-buffer-mode 'scratch-buffer)

  ;; Use TAB key to complete or indent the line/region
  (tab-always-indent 'complete)
  
  ;; Enable hungry deletion in programming modes
  (backward-delete-char-untabify-method 'all)

  ;; Better isearch movement
  (isearch-repeat-on-direction-change t)
  :bind
  (:map standard-keys-default-keymap
        ;; Unless you dont want to use tab-line, you can remove this line:
        ("C-t" . tab-line-new-tab)

        :map context-menu-mode-map
        ;; Bind Context Menu to `Apps' button
        ;; (requires context-menu-mode enabled)
        ("<apps>" . context-menu-open)

        :map help-map
        ;; Bind describe-face to C-h F
        ("F" . describe-face)

        ;; Make isearch easy to use
        :map isearch-mode-map
        ("<up>"   . isearch-repeat-backward)
        ("<down>" . isearch-repeat-forward)
        ("<remap> <yank>" . isearch-yank-kill)

        ;; Use RET as y (yes) action in y-or-n prompts
        :map y-or-n-p-map
        ("<return>" . y-or-n-p-insert-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional Package configurations, you can skip this if you don't want it.

;; Enable mouse right click menus
(context-menu-mode 1)

(use-package which-key
  :custom
  (which-key-add-column-padding 2)
  (which-key-dont-use-unicode)
  (which-key-ellipsis "..")
  (which-key-allow-multiple-replacements t)
  (which-key-idle-delay 0.5)
  :bind
  (:map which-key-C-h-map
        ("n" . nil) ("p" . nil)
        ("C-n" . nil) ("C-p" . nil)
        ("<left>" . which-key-show-next-page-cycle)
        ("<right>" . which-key-show-previous-page-cycle))
  :config
  (which-key-mode))

(use-package tab-line
  :hook (after-init . global-tab-line-mode)
  :bind
  (("C-<prior>" . tab-line-switch-to-prev-tab)
   ("C-<next>"  . tab-line-switch-to-next-tab)
   ("C-S-<prior>" . tab-line-move-tab-backward)
   ("C-S-<next>"  . tab-line-move-tab-forward))
  :custom
  (tab-line-new-tab-choice #'standard-keys-create-new-buffer)
  (tab-line-close-tab-function 'kill-buffer))

(use-package ready-player
  :config
  (setq ready-player-my-media-collection-location "/xtra/music")
  (ready-player-mode +1))

(use-package olivetti
  :defer t
  :init
  (defun my-center-ready-player-on ()
    "Enable olivetti-mode and visual-line-mode for ready-player."
    (setq olivetti-body-width 0) ; Or a number to control the amount of chars to be centered
    (olivetti-mode 1)            ; Enable olivetti-mode
    (visual-line-mode 1))        ; Enable/Disable visual-line-mode for text wrapping of your liking

  ;; Hook into ready-player's major mode
  (add-hook 'ready-player-major-mode-hook #'my-center-ready-player-on))

(use-package page-view)

(provide 'ui)
;;; ui.el ends here
