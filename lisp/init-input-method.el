;;; init-input-method.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Currently is all about Chinese.

;;; Code:

(elpaca pyim
        (require 'pyim)
        (setq default-input-method "pyim"))


(elpaca pyim-cangjiedict
        (require 'pyim-cangjiedict)
        (pyim-default-scheme 'cangjie)
        (pyim-cangjie3dict-enable)
        (pyim-cangjie5dict-enable))


;; This is for showing candidates.
(elpaca posframe
        (require 'posframe))

;; Input chinese punctuations with ease.
(require 'shapeless-chinese)
(defun drsl/toggle-shapeless-chinese ()
  (interactive)
  (if (equal shapeless-chinese-mode t)
      (setq shapeless-chinese-mode nil)
    (setq shapeless-chinese-mode t)))
(defun drsl/toggle-input-and-shapeless-chinese ()
  (interactive)
  (progn
    (drsl/toggle-shapeless-chinese)
    (toggle-input-method)))

(keymap-global-set "s-<SPC>" #'drsl/toggle-input-and-shapeless-chinese)
(keymap-global-set "C-z c" #'drsl/toggle-shapeless-chinese)

(provide 'init-input-method)
;;; init-input-method.el ends here
