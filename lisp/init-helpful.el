;;; init-helpful.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command))

(provide 'init-helpful)
;;; init-helpful.el ends here
