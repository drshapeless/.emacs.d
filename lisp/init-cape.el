;;; init-cape.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Completion backend other than company mode.

;;; Code:

;; Add extensions
(elpaca
    cape
  (require 'cape)
  (keymap-global-set "C-c p p" completion-at-point) ;; capf
  (keymap-global-set "C-c p t" complete-tag)        ;; etags
  (keymap-global-set "C-c p d" cape-dabbrev)        ;; or dabbrev-completion
  (keymap-global-set "C-c p h" cape-history)
  (keymap-global-set "C-c p f" cape-file)
  (keymap-global-set "C-c p k" cape-keyword)
  (keymap-global-set "C-c p s" cape-elisp-symbol)
  (keymap-global-set "C-c p e" cape-elisp-block)
  (keymap-global-set "C-c p a" cape-abbrev)
  (keymap-global-set "C-c p l" cape-line)
  (keymap-global-set "C-c p w" cape-dict)
  (keymap-global-set "C-c p :" cape-emoji)
  (keymap-global-set "C-c p \\" cape-tex)
  (keymap-global-set "C-c p _" cape-tex)
  (keymap-global-set "C-c p ^" cape-tex)
  (keymap-global-set "C-c p &" cape-sgml)
  (keymap-global-set "C-c p r" cape-rfc1345)
  (if *is-a-linux*
      ;; In Gentoo, install sys-apps/miscfiles.
      (setq ispell-alternate-dictionary "/usr/share/dict/web2"))
  )


(provide 'init-cape)
;;; init-cape.el ends here
