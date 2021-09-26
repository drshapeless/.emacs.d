;;; shapeless-theme.el --- Extreme Dark Theme for Emacs
;;
;; Filename: shapeless-theme.el
;; Description: The shapeless theme.
;; Author: Dr. ShapeLess <drsl@drshapeless.com>
;; Created: Sat Jan 23 14:05:48 2021 (+0800)
;; Modified: Fri Jun 4 12:46:25 2021 (+0800)
;; Version: 0.3.0

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

      ;; Other colors
      (sl/red           "#a10000")
      (sl/pale-orange   "#ceb058")
      (sl/bright-orange "#e09712")
      (sl/orange        "#b45900")
      (sl/yellow        "#cecb6e")
      (sl/green         "#32cd32")
      (sl/blue          "#4785ee")
      (sl/purple        "#ce66ce")
      (sl/violet        "#a63ea5")
      (sl/pink          "#d85daf")
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

   ;; system
   `(line-number-current-line ((t (:foreground ,sl/orange :background ,sl/dark-gray))))
   `(mode-line                ((t (:foreground ,sl/yellow :background ,sl/darker-gray))))
   `(mode-line-inactive       ((t (:foreground ,sl/white :background ,sl/darkest-gray))))
   `(minibuffer-prompt        ((t (:foreground ,sl/yellow))))

   ;; company
   `(company-tooltip           ((t (:foreground ,sl/yellow :background ,sl/darker-gray))))
   `(company-tooltip-common    ((t (:foreground ,sl/orange))))
   `(company-tooltip-selection ((t (:background ,sl/dark-gray))))
   `(company-scrollbar-bg      ((t (:background ,sl/dark-gray))))
   `(company-scrollbar-fg      ((t (:background ,sl/white))))
   `(company-preview-common    ((t (:inherit font-lock-comment-face))))

   ;; org
   `(org-block   ((t (:background ,sl/darkest-gray :extend t))))
   `(org-level-1 ((t (:foreground ,sl/green))))
   `(org-level-2 ((t (:foreground ,sl/bright-orange))))
   `(org-level-3 ((t (:foreground ,sl/yellow))))
   `(org-level-4 ((t (:foreground ,sl/blue))))
   `(org-table   ((t (:foreground ,sl/pale-orange))))
   ;; If your org file is more than 4 level, you are likely fucked up.

   ;; dired
   `(dired-directory ((t (:foreground ,sl/green))))

   ;; ivy
   `(ivy-current-match ((t (:foreground ,sl/yellow :background ,sl/dark-gray :extend t))))

   ;; comint
   `(comint-highlight-prompt ((t (:foreground ,sl/blue))))
   `(comint-highlight-input  ((t (:foreground ,sl/bright-orange))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,sl/blue))))

   ;; w3m
   `(w3m-anchor ((t (:foreground ,sl/blue :underline t))))

   ;; ansi-term
   ;; `(term ((t (:foreground ,sl/black :background ,sl/dark-gray))))
   `(term-color-black ((t (:foreground ,sl/white))))
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
