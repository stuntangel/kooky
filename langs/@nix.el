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

(use-package nix-ts-mode
  :unless noninteractive
  :config
  (def-auto-mode! nix-ts-mode "\\.nix\\'")
  (def-lsp! nix-ts-mode "nixd"))

(provide '@nix)

;;; @nix.el ends here
