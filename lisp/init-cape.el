;;; init-cape.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Completion backend other than company mode.

;;; Code:

;; Add extensions
(elpaca
    cape
  (require 'cape)

  (add-hook 'makefile-gmake-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

  (add-hook 'makefile-bsdmake-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

  (defcustom drsl/tailwind-css-keyword-file
    (expand-file-name "dict/tailwind_css_keyword.txt" user-emacs-directory)
    "tailwindcss keyword file path."
    :type 'string)

  (defun drsl/templ-ts-tailwind-cape-dict ()
    (when (drsl/is-class-attr)
      (setq-local cape-dict-file
                  drsl/tailwind-css-keyword-file)
      (pcase-let ((`(,beg . ,end) (cape--bounds 'word)))
        `(,beg ,end
               ,(cape--properties-table
                 (completion-table-case-fold
                  (cape--dynamic-table beg end #'cape--dict-list)
                  (not (cape--case-fold-p cape-dict-case-fold)))
                 :sort nil ;; Presorted word list (by frequency)
                 :category 'cape-dict)
               ,@cape--dict-properties))))

  (add-to-list 'drsl/eglot-extra-completion-functions
               #'drsl/templ-ts-tailwind-cape-dict))

(defun drsl/is-class-attr ()
  (let ((mynode (treesit-node-parent
                 (treesit-node-parent
                  (treesit-node-at (point))))))
    (if (and (string= (treesit-node-type mynode) "attribute")
             (string=
              (treesit-node-text (treesit-node-child mynode 0))
              "class"))
        t
      nil)))

(provide 'init-cape)
;;; init-cape.el ends here
