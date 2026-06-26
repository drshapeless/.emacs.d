;;; init-ghostel.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Another terminal

;;; Code:

(elpaca
    (ghostel :host github :repo "dakra/ghostel")
  (require 'ghostel)
  )

(provide 'init-ghostel)
;;; init-ghostel.el ends here
