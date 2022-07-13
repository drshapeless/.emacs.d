;;; init-consult.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; I don't know what consult does. It gives some better alternatives
;; of the built-in functions.

;;; Code:

(straight-use-package 'consult)
(require 'consult)
;; C-x bindings (ctl-x-map)
;; orig. repeat-complex-command
(keymap-global-set "C-x M-:" #'consult-complex-command)
;; orig. switch-to-buffer
(keymap-global-set "C-x b" #'consult-buffer)
;; orig. switch-to-buffer-other-window
(keymap-global-set "C-x 4 b" #'consult-buffer-other-window)
;; orig. switch-to-buffer-other-frame
(keymap-global-set "C-x 5 b" #'consult-buffer-other-frame)

;; Custom M-# bindings for fast register access
(keymap-global-set "M-#" #'consult-register-load)
;; orig. abbrev-prefix-mark (unrelated)
(keymap-global-set "M-'" #'consult-register-store)
(keymap-global-set "C-M-#" #'consult-register)

;; Other custom bindings
;; orig. yank-pop
(keymap-global-set "M-y" #'consult-yank-pop)
;; orig. apropos-command
(keymap-global-set "<help> a" #'consult-apropos)

;; M-g bindings (goto-map)
(keymap-global-set "M-g e" #'consult-compile-error)
;; Alternative: consult-flycheck
(keymap-global-set "M-g f" #'consult-flymake)
;; orig. goto-line
(keymap-global-set "M-g g" #'consult-goto-line)
;; orig. goto-line
(keymap-global-set "M-g M-g" #'consult-goto-line)
;; Alternative: consult-org-heading
(keymap-global-set "M-g o" #'consult-outline)
(keymap-global-set "M-g m" #'consult-mark)
(keymap-global-set "M-g k" #'consult-global-mark)
(keymap-global-set "M-g i" #'consult-imenu)
(keymap-global-set "M-g I" #'consult-imenu-multi)

;; M-s bindings (search-map)
(keymap-global-set "M-s f" #'consult-find)
(keymap-global-set "M-s F" #'consult-locate)
(keymap-global-set "M-s g" #'consult-grep)
(keymap-global-set "M-s G" #'consult-git-grep)
(keymap-global-set "M-s r" #'consult-ripgrep)
(keymap-global-set "M-s l" #'consult-line)
(keymap-global-set "M-s L" #'consult-line-multi)
(keymap-global-set "M-s m" #'consult-multi-occur)
(keymap-global-set "M-s k" #'consult-keep-lines)
(keymap-global-set "M-s u" #'consult-focus-lines)

;; Isearch integration
(keymap-global-set "M-s e" #'consult-isearch-history)
;; orig. isearch-edit-string
(keymap-set isearch-mode-map "M-e" #'consult-isearch-history)
;; orig. isearch-edit-string
(keymap-set isearch-mode-map "M-s e" #'consult-isearch-history)
;; needed by consult-line to detect isearch
(keymap-set isearch-mode-map "M-s l" #'consult-line)
(keymap-set isearch-mode-map "M-s L" #'consult-line-multi)

(add-hook 'completion-list-mode #'consult-preview-at-point-mode)

(setq register-preview-delay 0
      register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)

;; This is now deprecated.
;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(consult-customize
 consult-theme
 :preview-key '(:debounce 0.2 any)
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
 :preview-key (kbd "M-."))

(setq consult-narrow-key "<") ;; (kbd "C-+")

(setq consult-project-root-function (lambda ()
                                      (when-let (project (project-current))
                                        (car (project-roots project)))))

(provide 'init-consult)
;;; init-consult.el ends here
