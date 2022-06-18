;;; init-magit.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf magit
  :require t)

(leaf forge
  :after magit
  :require t)

(provide 'init-magit)
;;; init-magit.el ends here
