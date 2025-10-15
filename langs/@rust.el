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

(use-package rust-mode
  :unless noninteractive
  :config
  (def-auto-mode! rust-mode "\\.rs\\'")
  (def-lsp! rust-mode "rust-analyzer"))

(provide '@rust)

;;; @rust.el ends here
