;;; shapeless-prog-mode-helper.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Some helper utilities in prog-mode.

;; c-arrow-mode
;; Insert arrow operator easily.
;; Press "." twice to insert an arrow.

;; smart-semicolon
;; Press ";" twice to insert "::"

;;; Code:


(defun drsl/after-arrow ()
  (if (looking-back "->")
      t
    nil))

(defun shapeless-c-arrow-insert-dot ()
  (interactive)
  (if shapeless-c-arrow-p
      (let ((sp (syntax-ppss)))
        (cond ((drsl/after-arrow)
               (progn
                 (backward-delete-char 2)
                 (insert "...")))
              ((and (not (nth 4 sp))
                    (not (nth 3 sp))
                    (eq (char-before) ?\.))
               (progn
                 (backward-delete-char 1)
                 (insert "->")))
              (t (insert "."))))
    (self-insert-command 1 ?\.)))

(defun shapeless-smart-semicolon ()
  (interactive)
  (if shapeless-smart-semicolon-p
      (if (eq (char-before) ?\;)
          (progn (backward-delete-char 1)
                 (insert "::"))
        (insert ";"))
    (self-insert-command 1 ?\;)))

(defcustom shapeless-c-arrow-p t
  "Enable shapeless-c-arrow"
  :type 'boolean)

(defcustom shapeless-smart-semicolon-p t
  "Enable shapeless-smart-semicolon"
  :type 'boolean)

(define-minor-mode shapeless-prog-mode
  "prog-mode helper"
  :keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "." #'shapeless-c-arrow-insert-dot)
    (keymap-set map ";" #'shapeless-smart-semicolon)
    map))

(provide 'shapeless-prog-mode-helper)
;;; shapeless-prog-mode-helper.el ends here
