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

(use-package treesit
  :unless noninteractive
  :preface
  (defvar kooky--treesit-grammar-directory
    (concat-path kooky-cache-directory "tree-sitter")
    "Directory where Tree-sitter files are stored.")
  :config
  ;; This temporarily overrides `user-emacs-directory' to point to
  ;; `forge-cache-directory' so the `treesit' internals write the
  ;; compiled `*.so' files to our cache instead.
  (let ((overwrite-directory-fn (lambda (orig-fn &rest args)
              (let ((user-emacs-directory kooky-cache-directory))
                (apply orig-fn args)))))

  ;; Both functions rely on `user-emacs-directory' for determining where
  ;; to store the compiled grammar so we advise them to use our cache.
  (dolist (fn '(treesit-install-language-grammar treesit--build-grammar))
    (advice-add fn :around overwrite-directory-fn)))

  ;; Add the `kooky--treesit-grammar-directory' to the `treesit' load path.
  ;; This ensures that Tree-sitter can find the compiled grammars.
  (add-to-list 'treesit-extra-load-path kooky--treesit-grammar-directory))

(use-package treesit-auto
  :unless noninteractive
  :after treesit
  :preface
  (defvar kooky--treesit-auto-disabled-langs '(rust)
    "List of languages to exclude from `treesit-auto-langs'.")
  :config
  ;; Enable automatic installation of Tree-sitter grammars.
  ;; However - prompt for confirmation before doing so.
  (setq treesit-auto-install 'prompt)

  ;; Remove disabled languages from `treesit-auto'.
  (dolist (lang kooky--treesit-auto-disabled-langs)
    (setq treesit-auto-langs (delete lang treesit-auto-langs)))

  ;; Enable `global-treesit-auto-mode' after initialization.
  (add-hook 'after-init-hook #'global-treesit-auto-mode))

(provide '@treesit)

;;; @treesit.el ends here
