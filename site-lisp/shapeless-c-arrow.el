;;; shapeless-c-arrow.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Insert arrow operator easily.
;; Press "." twice to insert an arrow.

;;; Code:

(defun shapeless-c-arrow-insert-dot ()
  (interactive)
  (let ((sp (syntax-ppss)))
    (if (and (not (nth 4 sp))
             (not (nth 3 sp))
             (eq (char-before) ?\.))
        (progn
          (backward-delete-char 1)
          (insert "->"))
      (insert "."))))

(define-minor-mode shapeless-c-arrow-mode
  "Quickly insert arrow operator. ->"
  :keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "." #'shapeless-c-arrow-insert-dot)
    map))

(provide 'shapeless-c-arrow)
;;; shapeless-c-arrow.el ends here
