;;; init-jai.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Jai programming language

;;; Code:

(elpaca
    (jai-ts-mode :host github :repo "cpoile/jai-ts-mode")
  (require 'jai-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-ts-mode))

  ;; A temporary hack before my change got merged.
  (setq jai-ts-mode--indent-rules
        `((jai
           ;;((parent-is "source_file") column-0 0)
           ((node-is ")") parent-bol 0)
           ((node-is "]") parent-bol 0)
           ((node-is "}") parent-bol 0)
           ((parent-is "block") parent-bol jai-ts-mode-indent-offset)
           ((parent-is "struct_literal") parent-bol jai-ts-mode-indent-offset)
           ((parent-is "if_case_statement") parent-bol 0)  ;; Jai's style is no indent on cases?
           ((parent-is "if_statement") parent-bol jai-ts-mode-indent-offset)
           ((parent-is "named_parameters") parent-bol jai-ts-mode-indent-offset)
           ((match nil "assignment_parameters" nil 1 1) standalone-parent jai-ts-mode-indent-offset)
           ((match ")" "assignment_parameters" nil nil nil) standalone-parent 0)
           ((match nil "assignment_parameters" nil 2 nil) (nth-sibling 1) 0)
           ((parent-is "variable_declaration") parent-bol jai-ts-mode-indent-offset)
           ((parent-is "struct_declaration") parent-bol jai-ts-mode-indent-offset)
           ((parent-is "enum_declaration") parent-bol jai-ts-mode-indent-offset)
           ((parent-is "const_declaration") parent-bol jai-ts-mode-indent-offset)
           ((prev-line-is "switch_case") parent-bol jai-ts-mode-indent-offset)
           ((prev-line-is "statement") prev-line 0)
           (no-node parent-bol 0)))))

(provide 'init-jai)
;;; init-jai.el ends here
