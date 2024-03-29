;;; init-vertico.el --- vertico completion framework. -*- lexical-binding: t -*-

;;; Commentary:

;; This is better than ivy.

;;; Code:

(elpaca
 vertico
 (require 'vertico)
 (vertico-mode t)

 (require 'emacs)
 (defun crm-indicator (args)
   (cons (concat "[CRM] " (car args)) (cdr args)))
 (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

 ;; Do not allow the cursor in the minibuffer prompt
 (setq minibuffer-prompt-properties
       '(read-only t cursor-intangible t face minibuffer-prompt))

 (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
 ;; Enable recursive minibuffers
 (setq enable-recursive-minibuffers t)

 ;; This extensions comes with vertico installed by straight.
 (keymap-set vertico-map "RET" #'vertico-directory-enter)
 (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
 (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
 (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy))

(provide 'init-vertico)
;;; init-vertico.el ends here
