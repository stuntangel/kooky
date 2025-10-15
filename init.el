;;; init.el -*- lexical-binding: t; -*-
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

(defconst forge-modules
  '(;;
    ;; Core/Foundation modules.
    ;; These define the base editing environment and interface.
    (@macro @editor @evil @ui @complete @treesit @prog @tty @keymap)
    ;;
    ;; Language-specific modules.
    ;; These define programming language support and developer tools.
    (@nix @rust)
    )
  "List of modules to load during initialization.
Each symbol corresponds to a file (`*.el') with the same name.")

(when (not forge--nix)
  ;; Initialize and refresh `package.el' on non-Nix systems.
  ;; Since we disabled the automatic package initialization,
  ;; we need to explicitly bootstrap the package process.
  (require 'package)

  ;; Initialize only once - if not already done.
  (unless (bound-and-true-p package--initialized)
    (package-initialize))

  (unless (package-installed-p 'use-package)
    ;; Refresh only when the archive cache is empty.
    (when (not package-archive-contents)
      (package-refresh-contents))

    ;; Ensure that `use-package` is installed.
    (package-install 'use-package))

  (require 'use-package))

;; Load the modules declared in `forge-modules'.
;; Flattens any nested groups - then `require' each feature.
;; Each module file *must* contain a matching `provide' statement.
(mapc #'require (apply #'append forge-modules))

;; init.el ends here
