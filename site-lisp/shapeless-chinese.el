;;; shapeless-chinese.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defun drsl/quail-select-current-ignore-errors ()
  "`quail-select-current' but ignore any errors returned."
  (interactive)
  (ignore-errors
    (quail-select-current)))

(define-minor-mode shapeless-chinese-mode
  "Full width punctuation"
  :lighter " full-punc"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ",") (lambda () (interactive) (insert "，")))
    (define-key map (kbd ".") (lambda () (interactive) (insert "。")))
    (define-key map (kbd "[") (lambda () (interactive) (insert "「")))
    (define-key map (kbd "]") (lambda () (interactive) (insert "」")))
    (define-key map (kbd ":") (lambda () (interactive) (insert "：")))
    (define-key map (kbd ";") (lambda () (interactive) (insert "；")))
    (define-key map (kbd "?") (lambda () (interactive) (insert "？")))
    (define-key map (kbd "!") (lambda () (interactive) (insert "！")))
    (define-key map (kbd "\\") (lambda () (interactive) (insert "、")))
    (define-key map (kbd "(") (lambda () (interactive) (insert "（")))
    (define-key map (kbd ")") (lambda () (interactive) (insert "）")))
    ;; (define-key map (kbd "<") (lambda () (interactive) (insert "《")))
    ;; (define-key map (kbd ">") (lambda () (interactive) (insert "》")))
    ;; The following two is just some shortcuts for inserting some useful punctuation, who needs to insert ^ and ' in Chinese?
    ;; (define-key map (kbd "^") (lambda () (interactive) (insert "・")))
    ;; (define-key map (kbd "'") (lambda () (interactive) (insert "……")))
    ;; (define-key map (kbd "+") (lambda () (interactive) (insert " + ")))
    ;; (define-key map (kbd "-") (lambda () (interactive) (insert " - ")))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "SPC") #'drsl/quail-select-current-ignore-errors)
    map))

(provide 'shapeless-chinese)
;;; shapeless-chinese.el ends here
