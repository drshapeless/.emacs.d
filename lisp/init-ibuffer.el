;;; init-ibuffer.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf ibuffer-project
  :require t)

(defvar drsl/ibuffer-filter-groups
  '(("Helpful" (mode . helpful-mode))
    ("emacs" (or (name . "^\\*scratch\\*$")
                 (name . "^\\*Messages\\*$")
                 (name . "^\\*Warnings\\*$")
                 (name . "^\\*Quail Completions\\*$")
                 (name . "^\\*Async-native-compile-log\\*$")
                 (name . "^\\*Backtrace\\*$")
                 (name . "^\\*Native-compile-Log\\*$")
                 (name . "^\\*info\\*$")
                 (name . "^\\*straight-process\\*$")
                 (name . "^\\*v2ray\\*$")))
    ("eglot" (name . "\\^*EGLOT"))
    ))

(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (append drsl/ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

(setq ibuffer-formats
      '((mark " " (name 18 18 :left :elide)
              " " (human-readable-size 6 -1 :right)
              " " (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(define-ibuffer-column human-readable-size
  (:name "Size"
         :inline t
         :header-mouse-map ibuffer-size-header-map
         :summarizer
         (lambda (column-strings)
           (cl-loop for s in column-strings
                    sum (get-text-property (1- (length s)) 'size s) into total
                    finally return (file-size-human-readable total))))
  (let ((size (buffer-size)))
    (propertize (file-size-human-readable size)
                'size size)))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
