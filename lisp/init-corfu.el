;;; init-corfu.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Another completion framework other than company mode.
;; Corfu reuses a lot of internal Emacs ways of doing things.

;;; Code:

(elpaca
    corfu
  (require 'corfu)
  (setq corfu-auto t
        corfu-quit-no-match 'separator
        corfu-auto-prefix 1
        corfu-auto-delay 0.1
        corfu-min-width 80
        corfu-max-width corfu-min-width
        corfu-count 14
        corfu-scroll-margin 4
        corfu-cycle nil
        corfu-on-exact-match nil
        corfu-echo-documentation t
        text-mode-ispell-word-completion nil)
  (global-corfu-mode)

  ;; Corfu for eglot.
  (setq completion-category-overrides '((eglot (styles orderless))))

  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(provide 'init-corfu)
;;; init-corfu.el ends here
