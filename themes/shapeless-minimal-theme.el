;; shapeless-theme.el -- Extreme dark theme for Emacs.

;; Filename: shapeless-minimal-theme.el
;; Description: The shapeless minimal theme.
;; Author: Dr. ShapeLess <dr.shapeless@gmail.com>
;; Created: Sat Apr 23:20:42 2021 (+0800)
;; Modified: Sat Apr 23:20:42 2021 (+0800)
;; Version: 0.1.0

;;; Credits:

;;; Commentary:

;; This theme is for extreme black background, with extreme minimal colors.

;;; Code:
(require 'autothemer)

(autothemer-deftheme
 shapeless-minimal "The minimal version of shapeless theme for Emacs."

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
  (vertical-border                    (:foreground sl/dark-gray))

  ;; font-lock
  (font-lock-builtin-face            (:foreground sl/white :bold t))
  (font-lock-comment-delimiter-face  (:inherit font-lock-comment-face))
  (font-lock-comment-face            (:foreground sl/gray))
  (font-lock-constant-face           (:foreground sl/white :bold t))
  (font-lock-doc-face                (:foreground sl/white :bold t))
  (font-lock-function-name-face      (:foreground sl/white :bold t))
  (font-lock-keyword-face            (:foreground sl/white :bold t))
  (font-lock-negation-char-face      (:foreground sl/white :bold t))
  (font-lock-preprocessor-face       (:foreground sl/white :bold t))
  (font-lock-string-face             (:foreground sl/pale-orange))
  (font-lock-type-face               (:foreground sl/white :bold t))
  (font-lock-variable-name-face      (:foreground sl/bright-orange))
  (font-lock-warning-face            (:foreground sl/red :bold t))

  ;; system
  (line-number-current-line          (:foreground sl/orange :background sl/dark-gray))
  (mode-line                         (:foreground sl/yellow :background sl/darker-gray :bold t))
  (mode-line-inactive                (:foreground sl/white :background sl/darkest-gray :bold nil))
  (minibuffer-prompt                 (:foreground sl/yellow))

  ;; company
  (company-tooltip                   (:foreground sl/white :background sl/darker-gray))
  (company-tooltip-common            (:foreground sl/white :bold t))
  (company-tooltip-selection         (:background sl/dark-gray))
  (company-scrollbar-bg              (:background sl/dark-gray))
  (company-scrollbar-fg              (:background sl/white))
  (company-preview-common            (:inherit font-lock-comment-face))

  ;; org
  (org-block                         (:background sl/darkest-gray :extend t))
  (org-level-1                       (:foreground sl/white :bold t))
  (org-level-2                       (:foreground sl/white :bold t))
  (org-level-3                       (:foreground sl/white :bold t))
  (org-level-4                       (:foreground sl/white :bold t))

  ;; dired
  (dired-directory                   (:foreground sl/green))

  ;; ivy
  (ivy-current-match                 (:foreground sl/yellow :background sl/dark-gray :extend t))

  ;; comint
  (comint-highlight-prompt           (:foreground sl/blue))
  (comint-highlight-input            (:foreground sl/bright-orange))

  ;; eshell
  (eshell-prompt                     (:foreground sl/blue))

  ))


;;;###autoload
(defun shapeless-minimal-theme()
  "Apply the shapeless-minimal-theme"
  (interactive)
  (load-theme 'shapeless-minimal t))

(provide-theme 'shapeless-minimal)
;;; shapeless-theme.el ends here