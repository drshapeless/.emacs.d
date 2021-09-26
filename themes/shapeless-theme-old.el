;; shapeless-theme.el -- Extreme dark theme for Emacs.

;; Filename: shapeless-theme.el
;; Description: The shapeless theme.
;; Author: Dr. ShapeLess <dr.shapeless@gmail.com>
;; Created: Sat Jan 23 14:05:48 2021 (+0800)
;; Modified: Sat Mar 6 17:56:25 2021 (+0800)
;; Version: 0.2.0

;;; Credits:

;;; Commentary:

;; This theme is for extreme black background. Together with so low contract colors.

;;; Code:
(require 'autothemer)

(autothemer-deftheme
 shapeless "An extreme dark theme for Emacs."

 ((((class color) (min-colors #xffffff)))

  ;; Define color palette
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

 ;; Customize faces
 ((default                            (:foreground sl/white :background sl/black))
  (cursor                             (:background sl/orange))
  (region                             (:background sl/dark-gray))
  (fringe                             (:background sl/black))
  (highlight                          (:background sl/dark-gray))
  (link                               (:foreground sl/blue :underline t))
  (link-visited                       (:foreground sl/purple :underline t))
  (vertical-border                    (:foreground sl/dark-gray))

  ;; font-lock
  (font-lock-builtin-face            (:foreground sl/purple))
  (font-lock-comment-delimiter-face  (:inherit font-lock-comment-face))
  (font-lock-comment-face            (:foreground sl/gray))
  (font-lock-constant-face           (:foreground sl/violet))
  (font-lock-doc-face                (:foreground sl/yellow))
  (font-lock-function-name-face      (:foreground sl/green))
  (font-lock-keyword-face            (:foreground sl/blue))
  (font-lock-negation-char-face      (:foreground sl/yellow))
  (font-lock-preprocessor-face       (:foreground sl/yellow))
  (font-lock-string-face             (:foreground sl/pale-orange))
  (font-lock-type-face               (:foreground sl/pink))
  (font-lock-variable-name-face      (:foreground sl/bright-orange))
  (font-lock-warning-face            (:foreground sl/red :bold t))

  ;; system
  (line-number-current-line          (:foreground sl/orange :background sl/dark-gray))
  (mode-line                         (:foreground sl/yellow :background sl/darker-gray :bold t))
  (mode-line-inactive                (:foreground sl/white :background sl/darkest-gray :bold nil))
  (minibuffer-prompt                 (:foreground sl/yellow))

  ;; company
  (company-tooltip                   (:foreground sl/yellow :background sl/darker-gray))
  (company-tooltip-common            (:foreground sl/orange))
  (company-tooltip-selection         (:background sl/dark-gray))
  (company-scrollbar-bg              (:background sl/dark-gray))
  (company-scrollbar-fg              (:background sl/white))
  (company-preview-common            (:inherit font-lock-comment-face))

  ;; org
  (org-block                         (:background sl/darkest-gray :extend t))
  (org-level-1                       (:foreground sl/green))
  (org-level-2                       (:foreground sl/bright-orange))
  (org-level-3                       (:foreground sl/yellow))
  (org-level-4                       (:foreground sl/blue))
  (org-table                         (:foreground sl/pale-orange))

  ;; dired
  (dired-directory                   (:foreground sl/green))

  ;; ivy
  (ivy-current-match                 (:foreground sl/yellow :background sl/dark-gray :extend t))

  ;; comint
  (comint-highlight-prompt           (:foreground sl/blue))
  (comint-highlight-input            (:foreground sl/bright-orange))

  ;; eshell
  (eshell-prompt                     (:foreground sl/blue))

  ;; w3m
  (w3m-anchor                        (:foreground sl/blue :underline t))

  ))


;;;###autoload
(defun shapeless-theme()
  "Apply the shapeless-theme"
  (interactive)
  (load-theme 'shapeless t))

(provide-theme 'shapeless)
;;; shapeless-theme.el ends here
