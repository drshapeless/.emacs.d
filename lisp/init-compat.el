;;; init-compat.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; I am using the edge version of Emacs.

;; Compatibility functions for older Emacs to use the config.

;;; Code:

(if *is-older-emacs*
    (progn
      (defun drsl/keymap-set (keymap key definition)
        "Workaround of keymap-set in older Emacs.

This does not check the validity of key string."
        (define-key keymap (kbd key) definition))

      (defun drsl/keymap-unset (keymap key &optional remove)
        "Workaround of keymap-unset in older Emacs.

This does not check the validity of key string."
        ;; Older define-key only accept 3 arguments.
        (define-key keymap (kbd key) nil))

      (defun drsl/keymap-global-set (key command)
        "Workaround of keymap-global-set in older Emacs.

This does not check the validity of key string."
        (drsl/keymap-set (current-global-map) key command))

      (defun drsl/keymap-global-unset (key &optional remove)
        "Workaround of keymap-global-unset in older Emacs.

This does not check the validity of key string."
        (drsl/keymap-unset (current-global-map) key remove))

      (defalias 'keymap-set #'drsl/keymap-set)
      (defalias 'keymap-unset #'drsl/keymap-unset)
      (defalias 'keymap-global-set #'drsl/keymap-global-set)
      (defalias 'keymap-global-unset #'drsl/keymap-global-unset))
  )

(provide 'init-compat)
;;; init-compat.el ends here
