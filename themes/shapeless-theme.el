;;; shapeless-theme.el --- Extreme Dark Theme for Emacs
;;
;; Filename: shapeless-theme.el
;; Description: The shapeless theme.
;; Author: DrShapeLess <drsl@drshapeless.com>
;; Created: Sat Jan 23 14:05:48 2021 (+0800)
;; Modified: Mon June 6 10:11:00 2022 (+0800)
;; Version: 0.4.0

;;; Commentary:

;; shapeless-theme is a personal theme made by Dr. ShapeLess.

;; It features an extreme dark background with a calm color palette,
;; which is designed for people working in extrememly dark room.


;;; Code:

(deftheme shapeless "An extremely dark theme.")

;; Define a color palette.
(let (
      ;; Gray scales
      (sl/black         "#000000")
      (sl/white         "#cecece")
      (sl/gray          "#6e6e6e")
      (sl/dark-gray     "#404040")
      (sl/darker-gray   "#2b2b2b")
      (sl/darkest-gray  "#1a1a1a")
      (sl/weird-bg      "#191a1b")

      ;; Other colors
      (sl/red           "#a10000")
      (sl/pale-orange   "#ceb058")
      (sl/bright-orange "#e09712")
      (sl/orange        "#b45900")
      (sl/brown         "#542121")
      (sl/yellow        "#cecb6e")
      (sl/green         "#32cd32")
      (sl/blue          "#4785ee")
      (sl/purple        "#ce66ce")
      (sl/violet        "#a63ea5")
      (sl/pink          "#d85daf")
      (sl/blue-green    "#00415e")
      (sl/light-blue    "#c0efff")
      )

  ;; Customize faces.
  (custom-theme-set-faces
   `shapeless
   `(default         ((t (:foreground ,sl/white :background ,sl/black))))
   `(cursor          ((t (:background ,sl/orange))))
   `(region          ((t (:background ,sl/dark-gray))))
   `(fringe          ((t (:background ,sl/black))))
   `(highlight       ((t (:background ,sl/dark-gray))))
   `(link            ((t (:foreground ,sl/blue :underline t))))
   `(link-visited    ((t (:foreground ,sl/purple :underline t))))
   `(vertical-border ((t (:foreground ,sl/dark-gray))))

   ;; font lock
   `(font-lock-builtin-face           ((t (:foreground ,sl/purple))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-comment-face           ((t (:foreground ,sl/gray))))
   `(font-lock-constant-face          ((t (:foreground ,sl/violet))))
   `(font-lock-doc-face               ((t (:foreground ,sl/yellow))))
   `(font-lock-function-name-face     ((t (:foreground ,sl/green))))
   `(font-lock-keyword-face           ((t (:foreground ,sl/blue))))
   `(font-lock-negation-char-face     ((t (:foreground ,sl/yellow))))
   `(font-lock-preprocessor-face      ((t (:foreground ,sl/yellow))))
   `(font-lock-string-face            ((t (:foreground ,sl/pale-orange))))
   `(font-lock-type-face              ((t (:foreground ,sl/pink))))
   `(font-lock-variable-name-face     ((t (:foreground
                                           ,sl/bright-orange))))
   `(font-lock-warning-face           ((t (:foreground ,sl/red :bold t))))

   ;; tree-sitter
   `(tree-sitter-hl-face:attribute             ((t (:foreground ,sl/yellow))))
   `(tree-sitter-hl-face:comment               ((t (:foreground ,sl/gray))))
   `(tree-sitter-hl-face:constant              ((t (:foreground ,sl/violet))))
   `(tree-sitter-hl-face:constant.builtin      ((t (:foreground ,sl/violet))))
   `(tree-sitter-hl-face:constructor           ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:doc                   ((t (:foreground ,sl/yellow))))
   `(tree-sitter-hl-face:embedded              ((t (:foreground ,sl/white))))
   `(tree-sitter-hl-face:escape                ((t (:foreground ,sl/violet))))
   `(tree-sitter-hl-face:function              ((t (:foreground ,sl/green))))
   `(tree-sitter-hl-face:function.builtin      ((t (:foreground ,sl/green))))
   `(tree-sitter-hl-face:function.call         ((t (:foreground ,sl/green))))
   `(tree-sitter-hl-face:function.macro        ((t (:foreground ,sl/green))))
   `(tree-sitter-hl-face:function.special      ((t (:foreground ,sl/green))))
   `(tree-sitter-hl-face:keyword               ((t (:foreground ,sl/blue))))
   `(tree-sitter-hl-face:label                 ((t (:foreground ,sl/yellow))))
   `(tree-sitter-hl-face:method                ((t (:foreground ,sl/green))))
   `(tree-sitter-hl-face:method.call           ((t (:foreground ,sl/green))))
   `(tree-sitter-hl-face:number                ((t (:foreground ,sl/yellow))))
   `(tree-sitter-hl-face:operator              ((t (:foreground ,sl/blue))))
   `(tree-sitter-hl-face:property              ((t (:foreground ,sl/bright-orange))))
   `(tree-sitter-hl-face:property.definition   ((t (:foreground ,sl/yellow))))
   `(tree-sitter-hl-face:punctuation           ((t (:foreground ,sl/white))))
   `(tree-sitter-hl-face:punctuation.bracket   ((t (:foreground ,sl/white))))
   `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,sl/white))))
   `(tree-sitter-hl-face:punctuation.special   ((t (:foreground ,sl/white))))
   `(tree-sitter-hl-face:string                ((t (:foreground ,sl/pale-orange))))
   `(tree-sitter-hl-face:string.special        ((t (:foreground ,sl/pale-orange))))
   `(tree-sitter-hl-face:tag                   ((t (:foreground ,sl/yellow))))
   `(tree-sitter-hl-face:type                  ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.argument         ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.builtin          ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.parameter        ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.super            ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:variable              ((t (:foreground ,sl/bright-orange))))
   `(tree-sitter-hl-face:variable.builtin      ((t (:foreground ,sl/blue))))
   `(tree-sitter-hl-face:variable.parameter    ((t (:foreground ,sl/bright-orange))))
   `(tree-sitter-hl-face:variable.special      ((t (:foreground ,sl/bright-orange))))

   ;; system
   `(line-number-current-line ((t (:foreground ,sl/orange :background ,sl/dark-gray))))
   `(mode-line                ((t (:foreground ,sl/yellow :background ,sl/darker-gray))))
   `(mode-line-active         ((t (:inherit mode-line))))
   `(mode-line-inactive       ((t (:foreground ,sl/white :background ,sl/darkest-gray))))
   `(minibuffer-prompt        ((t (:foreground ,sl/yellow))))

   ;; company
   `(company-tooltip           ((t (:background ,sl/weird-bg))))
   `(company-tooltip-common    ((t (:foreground ,sl/orange))))
   `(company-tooltip-selection ((t (:background ,sl/blue-green))))
   `(company-scrollbar-bg      ((t (:background ,sl/dark-gray))))
   `(company-scrollbar-fg      ((t (:background ,sl/white))))
   `(company-preview-common    ((t (:inherit font-lock-comment-face))))
   `(company-template-field    ((t (:foreground ,sl/white :background ,sl/brown))))

   ;; org
   `(org-block   ((t (:background ,sl/darkest-gray :extend t))))
   `(org-level-1 ((t (:foreground ,sl/green))))
   `(org-level-2 ((t (:foreground ,sl/bright-orange))))
   `(org-level-3 ((t (:foreground ,sl/yellow))))
   `(org-level-4 ((t (:foreground ,sl/blue))))
   `(org-table   ((t (:foreground ,sl/pale-orange))))
   `(org-meta-line ((t (:inherit font-lock-comment-face))))
   `(org-code    ((t (:background ,sl/darkest-gray))))
   `(org-verbatim ((t (:background ,sl/darkest-gray))))
   `(org-date    ((t (:foreground ,sl/pink))))
   ;; If your org file is more than 4 level, you are likely fucked up.

   ;; dired
   `(dired-directory ((t (:foreground ,sl/green))))

   ;; ivy
   `(ivy-current-match ((t (:foreground ,sl/yellow :background ,sl/dark-gray :extend t))))

   ;; vertico
   `(vertico-current ((t (:foreground ,sl/yellow :background ,sl/dark-gray :extend t))))

   ;; comint
   `(comint-highlight-prompt ((t (:foreground ,sl/blue))))
   `(comint-highlight-input  ((t (:foreground ,sl/bright-orange))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,sl/blue))))

   ;; w3m
   `(w3m-anchor ((t (:foreground ,sl/blue :underline t))))

   ;; ansi-term
   ;; `(term ((t (:foreground ,sl/black :background ,sl/dark-gray))))
   ;; `(term-color-black ((t (:foreground ,sl/white))))

   ;; vterm
   `(vterm-color-blue ((t (:foreground ,sl/blue))))

   ;; markdown
   `(markdown-inline-code-face ((t (:foreground ,sl/bright-orange))))
   `(markdown-pre-face         ((t (:foreground ,sl/yellow))))

   ;; web
   ;; `(web-mode-annotation-face ((t (:foreground ,sl/green))))
   ;; `(web-mode-comment-face    ((t (:foreground ,sl/green))))
   `(web-mode-html-tag-face   ((t (:foreground ,sl/green))))
   `(web-mode-html-attr-name-face ((t (:foreground ,sl/bright-orange))))
   )                                    ; end of custom-theme-set-faces

  (custom-theme-set-variables
   'shapeless
   `(ansi-color-names-vector
     [,sl/darker-gray ,sl/red ,sl/green ,sl/yellow ,sl/blue ,sl/purple ,sl/blue ,sl/white]))
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun shapeless-theme()
  "Apply the shapeless-theme"
  (interactive)
  (load-theme 'shapeless t))

(provide-theme 'shapeless)
;;; shapeless-theme.el ends here
