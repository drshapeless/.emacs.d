;;; shapeless-theme.el --- Extreme Dark Theme for Emacs
;;
;; Filename: shapeless-theme.el
;; Description: The shapeless theme.
;; Author: drshapeless <drsl@drshapeless.com>
;; Created: Sat Jan 23 14:05:48 2021 (+0800)
;; Modified: Tue May 23 20:12:38 2023 (+0800)
;; Version: 0.6.1

;;; Commentary:

;; shapeless-theme is a personal theme made by Dr. ShapeLess.

;; It features an extreme dark background with a calm color palette,
;; which is designed for people working in extrememly dark room.


;;; Code:

(deftheme shapeless "An extremely dark theme.")

;; Define a color palette.
(let (
      ;; Gray scales
      (sl/calm-black         "#000000")
      (sl/calm-white         "#cecece")
      (sl/gray          "#6e6e6e")
      (sl/dark-gray     "#404040")
      (sl/darker-gray   "#2b2b2b")
      (sl/darkest-gray  "#1a1a1a")
      (sl/weird-bg      "#191a1b")

      ;; Other colors
      (sl/calm-red      "#a10000")
      (sl/pale-orange   "#ceb058")
      (sl/bright-orange "#e09712")
      (sl/orange        "#b45900")
      (sl/brown         "#542121")
      (sl/calm-yellow   "#cecb6e")
      (sl/calm-green    "#32cd32")
      (sl/calm-blue     "#4785ee")
      (sl/dodger-blue   "#1E90FF")
      (sl/purple        "#ce66ce")
      (sl/violet        "#a63ea5")
      (sl/pink          "#d85daf")
      (sl/blue-green    "#00415e")
      (sl/light-blue    "#c0efff")
      (sl/weird-green   "#306C53")
      (sl/weirder-green "#214c3a")
      (sl/crazy-green   "#5f93a0")

      ;; ansi-color-name
      (sl/black         "#000000")
      (sl/blue          "#055C9D")
      (sl/cyan          "#5CFFFF")
      (sl/green         "#5CFF5C")
      (sl/magenta       "#D100D1")
      (sl/red           "#A60027")
      (sl/white         "#D1D1D1")
      (sl/yellow        "#D7D704")

      (sl/bright-black   "#2b2b2b")
      (sl/bright-blue    "#0E86D4")
      (sl/bright-cyan    "#00FFFF")
      (sl/bright-green   "#2EFF2E")
      (sl/bright-magenta "#FF0BAC")
      (sl/bright-red     "#FF033E")
      (sl/bright-white   "#E8E8E8")
      (sl/bright-yellow  "#FAFA0F")
      )

  ;; Customize faces.
  (custom-theme-set-faces
   `shapeless
   `(default         ((t (:foreground ,sl/calm-white :background ,sl/calm-black))))
   `(cursor          ((t (:background ,sl/orange))))
   `(region          ((t (:background ,sl/dark-gray))))
   `(fringe          ((t (:background ,sl/calm-black))))
   `(highlight       ((t (:background ,sl/dark-gray))))
   `(link            ((t (:foreground ,sl/calm-blue :underline t))))
   `(link-visited    ((t (:foreground ,sl/purple :underline t))))
   `(vertical-border ((t (:foreground ,sl/dark-gray))))

   ;; font lock
   `(font-lock-bracket-face              ((t (:foreground ,sl/calm-white))))
   `(font-lock-builtin-face              ((t (:foreground ,sl/purple))))
   `(font-lock-comment-delimiter-face    ((t (:inherit font-lock-comment-face))))
   `(font-lock-comment-face              ((t (:foreground ,sl/gray))))
   `(font-lock-constant-face             ((t (:foreground ,sl/violet))))
   `(font-lock-doc-face                  ((t (:foreground ,sl/calm-yellow))))
   `(font-lock-doc-markup-face           ((t (:foreground ,sl/calm-yellow))))
   `(font-lock-escape-face               ((t (:foreground ,sl/calm-white))))
   `(font-lock-function-name-face        ((t (:foreground ,sl/calm-green))))
   `(font-lock-keyword-face              ((t (:foreground ,sl/dodger-blue))))
   `(font-lock-misc-punctuation-face     ((t (:foreground ,sl/calm-white))))
   `(font-lock-negation-char-face        ((t (:foreground ,sl/calm-yellow))))
   `(font-lock-number-face               ((t (:foreground ,sl/calm-yellow))))
   `(font-lock-operator-face             ((t (:foreground ,sl/dodger-blue))))
   `(font-lock-preprocessor-face         ((t (:foreground ,sl/calm-yellow))))
   `(font-lock-property-face             ((t (:foreground ,sl/crazy-green))))
   `(font-lock-punctuation-face          ((t (:foreground ,sl/calm-white))))
   `(font-lock-regexp-face               ((t (:foreground ,sl/pale-orange))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,sl/calm-white :bold t))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,sl/calm-white :bold t))))
   `(font-lock-string-face               ((t (:foreground ,sl/pale-orange))))
   `(font-lock-type-face                 ((t (:foreground ,sl/pink))))
   `(font-lock-variable-name-face        ((t (:foreground ,sl/bright-orange))))
   `(font-lock-warning-face              ((t (:foreground ,sl/calm-red :bold t))))

   ;; tree-sitter
   `(tree-sitter-hl-face:attribute             ((t (:foreground ,sl/calm-yellow))))
   `(tree-sitter-hl-face:comment               ((t (:foreground ,sl/gray))))
   `(tree-sitter-hl-face:constant              ((t (:foreground ,sl/violet))))
   `(tree-sitter-hl-face:constant.builtin      ((t (:foreground ,sl/violet))))
   `(tree-sitter-hl-face:constructor           ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:doc                   ((t (:foreground ,sl/calm-yellow))))
   `(tree-sitter-hl-face:embedded              ((t (:foreground ,sl/calm-white))))
   `(tree-sitter-hl-face:escape                ((t (:foreground ,sl/violet))))
   `(tree-sitter-hl-face:function              ((t (:foreground ,sl/calm-green))))
   `(tree-sitter-hl-face:function.builtin      ((t (:foreground ,sl/calm-green))))
   `(tree-sitter-hl-face:function.call         ((t (:foreground ,sl/calm-green))))
   `(tree-sitter-hl-face:function.macro        ((t (:foreground ,sl/calm-green))))
   `(tree-sitter-hl-face:function.special      ((t (:foreground ,sl/calm-green))))
   `(tree-sitter-hl-face:keyword               ((t (:foreground ,sl/dodger-blue))))
   `(tree-sitter-hl-face:label                 ((t (:foreground ,sl/calm-yellow))))
   `(tree-sitter-hl-face:method                ((t (:foreground ,sl/calm-green))))
   `(tree-sitter-hl-face:method.call           ((t (:foreground ,sl/calm-green))))
   `(tree-sitter-hl-face:number                ((t (:foreground ,sl/calm-yellow))))
   `(tree-sitter-hl-face:operator              ((t (:foreground ,sl/dodger-blue))))
   `(tree-sitter-hl-face:property              ((t (:foreground ,sl/bright-orange))))
   `(tree-sitter-hl-face:property.definition   ((t (:foreground ,sl/calm-yellow))))
   `(tree-sitter-hl-face:punctuation           ((t (:foreground ,sl/calm-white))))
   `(tree-sitter-hl-face:punctuation.bracket   ((t (:foreground ,sl/calm-white))))
   `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,sl/calm-white))))
   `(tree-sitter-hl-face:punctuation.special   ((t (:foreground ,sl/calm-white))))
   `(tree-sitter-hl-face:string                ((t (:foreground ,sl/pale-orange))))
   `(tree-sitter-hl-face:string.special        ((t (:foreground ,sl/pale-orange))))
   `(tree-sitter-hl-face:tag                   ((t (:foreground ,sl/calm-yellow))))
   `(tree-sitter-hl-face:type                  ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.argument         ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.builtin          ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.parameter        ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:type.super            ((t (:foreground ,sl/pink))))
   `(tree-sitter-hl-face:variable              ((t (:foreground ,sl/bright-orange))))
   `(tree-sitter-hl-face:variable.builtin      ((t (:foreground ,sl/dodger-blue))))
   `(tree-sitter-hl-face:variable.parameter    ((t (:foreground ,sl/bright-orange))))
   `(tree-sitter-hl-face:variable.special      ((t (:foreground ,sl/bright-orange))))

   ;; system
   `(line-number-current-line ((t (:foreground ,sl/orange :background ,sl/dark-gray))))
   `(mode-line                ((t (:foreground ,sl/calm-yellow :background ,sl/darker-gray))))
   `(mode-line-active         ((t (:inherit mode-line))))
   `(mode-line-inactive       ((t (:foreground ,sl/calm-white :background ,sl/darkest-gray))))
   `(minibuffer-prompt        ((t (:foreground ,sl/calm-yellow))))

   ;; company
   `(company-tooltip           ((t (:background ,sl/weird-bg))))
   `(company-tooltip-common    ((t (:foreground ,sl/orange))))
   `(company-tooltip-selection ((t (:background ,sl/blue-green))))
   `(company-scrollbar-bg      ((t (:background ,sl/dark-gray))))
   `(company-scrollbar-fg      ((t (:background ,sl/calm-white))))
   `(company-preview-common    ((t (:inherit font-lock-comment-face))))
   `(company-template-field    ((t (:foreground ,sl/calm-white :background ,sl/brown))))

   ;; org
   `(org-block   ((t (:background ,sl/darkest-gray :extend t))))
   `(org-level-1 ((t (:foreground ,sl/calm-green :weight ultra-bold))))
   `(org-level-2 ((t (:foreground ,sl/bright-orange :weight extra-bold))))
   `(org-level-3 ((t (:foreground ,sl/purple))))
   `(org-level-4 ((t (:foreground ,sl/dodger-blue))))
   `(org-table   ((t (:foreground ,sl/pale-orange))))
   `(org-meta-line ((t (:inherit font-lock-comment-face))))
   `(org-code    ((t (:background ,sl/darkest-gray))))
   `(org-verbatim ((t (:background ,sl/darkest-gray))))
   `(org-date    ((t (:foreground ,sl/pink))))
   ;; If your org file is more than 4 level, you are likely fucked up.

   ;; dired
   `(dired-directory ((t (:foreground ,sl/calm-green))))

   ;; ivy
   `(ivy-current-match ((t (:foreground ,sl/calm-yellow :background ,sl/dark-gray :extend t))))

   ;; vertico
   `(vertico-current ((t (:foreground ,sl/calm-yellow :background ,sl/dark-gray :extend t))))

   ;; comint
   `(comint-highlight-prompt ((t (:foreground ,sl/dodger-blue))))
   `(comint-highlight-input  ((t (:foreground ,sl/bright-orange))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,sl/dodger-blue))))

   ;; w3m
   `(w3m-anchor ((t (:foreground ,sl/calm-blue :underline t))))

   ;; ansi-color
   `(ansi-color-bold  ((t (:inherit bold))))
   `(ansi-color-black ((t (:foreground ,sl/black :background ,sl/black))))
   `(ansi-color-blue  ((t (:foreground ,sl/blue :background ,sl/blue))))
   `(ansi-color-cyan  ((t (:foreground ,sl/cyan :background ,sl/cyan))))
   `(ansi-color-green  ((t (:foreground ,sl/green :background ,sl/green))))
   `(ansi-color-magenta  ((t (:foreground ,sl/magenta :background ,sl/magenta))))
   `(ansi-color-red  ((t (:foreground ,sl/red :background ,sl/red))))
   `(ansi-color-white  ((t (:foreground ,sl/white :background ,sl/white))))
   `(ansi-color-yellow  ((t (:foreground ,sl/yellow :background ,sl/yellow))))
   `(ansi-color-bright-black  ((t (:foreground ,sl/bright-black :background ,sl/bright-black))))
   `(ansi-color-bright-blue  ((t (:foreground ,sl/bright-blue :background ,sl/bright-blue))))
   `(ansi-color-bright-cyan  ((t (:foreground ,sl/bright-cyan :background ,sl/bright-cyan))))
   `(ansi-color-bright-green  ((t (:foreground ,sl/bright-green :background ,sl/bright-green))))
   `(ansi-color-bright-magenta  ((t (:foreground ,sl/bright-magenta :background ,sl/bright-magenta))))
   `(ansi-color-bright-red  ((t (:foreground ,sl/bright-red :background ,sl/bright-red))))
   `(ansi-color-bright-white  ((t (:foreground ,sl/bright-white :background ,sl/bright-white))))
   `(ansi-color-bright-yellow  ((t (:foreground ,sl/bright-yellow :background ,sl/bright-yellow))))

   ;; ansi-term
   ;; `(term ((t (:foreground ,sl/calm-black :background ,sl/dark-gray))))
   ;; `(term-color-black ((t (:foreground ,sl/calm-white))))

   ;; vterm
   `(vterm-color-blue ((t (:foreground ,sl/dodger-blue))))

   ;; markdown
   `(markdown-inline-code-face ((t (:foreground ,sl/bright-orange))))
   `(markdown-pre-face         ((t (:foreground ,sl/calm-yellow))))

   ;; web
   ;; `(web-mode-annotation-face ((t (:foreground ,sl/calm-green))))
   ;; `(web-mode-comment-face    ((t (:foreground ,sl/calm-green))))
   `(web-mode-html-tag-face   ((t (:foreground ,sl/calm-green))))
   `(web-mode-html-attr-name-face ((t (:foreground ,sl/bright-orange))))

   ;; highlight-indent-guides
   `(highlight-indent-guides-character-face ((t (:foreground ,sl/weird-green))))
   `(highlight-indent-guides-odd-face       ((t (:background ,sl/weird-green))))
   `(highlight-indent-guides-even-face      ((t (:background ,sl/weirder-green))))

   ;; indent-guide
   `(indent-guide-face ((t (:foreground ,sl/weird-green))))
   )                                    ; end of custom-theme-set-faces

  ;; (custom-theme-set-variables
  ;;  'shapeless
  ;;  `(ansi-color-names-vector
  ;;    [,sl/darker-gray ,sl/calm-red ,sl/calm-green ,sl/calm-yellow ,sl/calm-blue ,sl/purple ,sl/calm-blue ,sl/calm-white]))
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
