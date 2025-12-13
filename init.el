;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;; Author: stuntangel
;; Version: 2.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Modern modular Emacs configuration for enhanced development experience.
;; Configuration is split into logical modules in the elisp/ directory.

;;; Code:

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Package.el provides metadata only - packages are pre-installed by Nix
(require 'package)
(setq package-archives nil)           ; Disable archives BEFORE initialization
(setq package-archive-priorities nil) ; Disable archive priorities
(setq package-vc-heuristic-alist nil) ; Disable VC package detection (Emacs 29+)
(package-initialize)                  ; Initialize with no remote archives

;; Store automatic customization options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; Load configuration modules
(require 'fonts)       ; Font configuration and management
(require 'ui) ; ui

(require 'nano) ; nano
(nano-light) ; initialize to light theme

;; Additional GC optimizations from Doom Emacs patterns
;; Trigger GC when idle for 5 seconds
(run-with-idle-timer 5 t #'garbage-collect)

;; Prevent GC during minibuffer operations (completion!)
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(provide 'init)
;;; init.el ends here
