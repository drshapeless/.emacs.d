;;; full-width-punc.el --- A minor mode for inputing chinese punctuation.
;;; Commentary:

;; My own implementation of inputing full width char.

;;; Code:

(define-minor-mode drsl/full-width-punc-mode
  "Full width punctuation"
  :lighter " full-punc"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ",") (lambda () (interactive) (insert "，")))
    (define-key map (kbd ".") (lambda () (interactive) (insert "。")))
    ;; (define-key map (kbd "[") (lambda () (interactive) (insert "「")))
    ;; (define-key map (kbd "]") (lambda () (interactive) (insert "」")))
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
    ;; (define-key map (kbd "'") (lambda () (interactive) (insert "⋯⋯")))
    ;; (define-key map (kbd "+") (lambda () (interactive) (insert " + ")))
    ;; (define-key map (kbd "-") (lambda () (interactive) (insert " - ")))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "SPC") 'quail-select-current )
    map))

(provide 'full-width-punc)
;;; full-width-punc.el ends here
