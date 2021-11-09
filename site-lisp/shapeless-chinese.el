;;; shapeless-chinese.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

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
    (define-key map (kbd "SPC") 'quail-select-current )
    map))


(provide 'shapeless-chinese)
;;; shapeless-chinese.el ends here
