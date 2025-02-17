;;; init-apheleia.el --- Format code -*- lexical-binding: t -*-

;;; Commentary:

;; This is an alternative to format-all.

;;; Code:

(elpaca
    apheleia
  (require 'apheleia)

  (add-to-list 'apheleia-mode-alist
               '(swift-mode . swift-format))
  (add-to-list 'apheleia-mode-alist
               '(templ-ts-mode . templ-format))
  (add-to-list 'apheleia-mode-alist
               '(odin-ts-mode . odin-format))

  (add-to-list 'apheleia-formatters
               '(swift-format "swift-format" (buffer-file-name)))
  (add-to-list 'apheleia-formatters
               '(templ-format "templ" "fmt"))
  (add-to-list 'apheleia-formatters
               '(odin-format "odinfmt" "-stdin"))

  (apheleia-global-mode 1))

(provide 'init-apheleia)
;;; init-apheleia.el ends here
