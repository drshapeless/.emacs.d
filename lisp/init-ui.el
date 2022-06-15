;;; init-ui.el --- User interface config. -*- lexical-binding: t -*-

;;; Commentary:

;; User interface settings.

;;; Code:

;; Use menlo as default font.
;; (set-frame-font "menlo" nil t)
(defun drsl/set-font-size ()
  (lambda () (set-face-attribute 'default nil :height 120)))
;; (set-face-attribute 'default nil :height 120)
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(setq after-load-theme-hook (lambda () (set-face-attribute 'default nil :height 120)))

(setq use-default-font-for-symbols nil)

(defun drsl/init-font ()
  (progn
    ;; (set-face-font
    ;;  'default
    ;;  (font-spec
    ;;   :family "menlo"
    ;;   :size 18
    ;;   :weight 'normal
    ;;   :width 'normal
    ;;   :slant 'normal))
    (set-face-attribute 'default nil :font "menlo" :height 120)

    ;; Provided by Sebastian Urban
    ;; More information at https://idiocy.org/emacs-fonts-and-fontsets.html
    (set-fontset-font "fontset-default" 'adlam "Noto Sans Adlam")
    (set-fontset-font "fontset-default" 'anatolian "Noto Sans Anatolian Hieroglyphs")
    (set-fontset-font "fontset-default" 'arabic "Noto Sans Arabic")
    (set-fontset-font "fontset-default" 'aramaic "Noto Sans Imperial Aramaic Regular")
    (set-fontset-font "fontset-default" 'armenian "Noto Sans Armenian")
    (set-fontset-font "fontset-default" 'avestan "Noto Sans Avestan")
    (set-fontset-font "fontset-default" 'balinese "Noto Sans Balinese")
    (set-fontset-font "fontset-default" 'bamum "Noto Sans Bamum")
    (set-fontset-font "fontset-default" 'batak "Noto Sans Batak")
    (set-fontset-font "fontset-default" 'bengali "Noto Sans Bengali")
    (set-fontset-font "fontset-default" 'brahmi "Noto Sans Brahmi")
    (set-fontset-font "fontset-default" 'buginese "Noto Sans Buginese")
    (set-fontset-font "fontset-default" 'buhid "Noto Sans Buhid")
    (set-fontset-font "fontset-default" 'burmese "Noto Sans Myanmar")
    (set-fontset-font "fontset-default" 'canadian-aboriginal "Noto Sans Canadian Aboriginal")
    (set-fontset-font "fontset-default" 'carian "Noto Sans Carian")
    (set-fontset-font "fontset-default" 'chakma "Noto Sans Chakma")
    (set-fontset-font "fontset-default" 'cham "Noto Sans Cham")
    (set-fontset-font "fontset-default" 'cherokee "Noto Sans Cherokee")
    (set-fontset-font "fontset-default" 'cjk-misc "Noto Sans CJK SC Regular")
    (set-fontset-font "fontset-default" 'coptic "Noto Sans Coptic Regular")
    (set-fontset-font "fontset-default" 'cuneiform "Noto Sans Cuneiform")
    (set-fontset-font "fontset-default" 'cypriot-syllabary "Noto Sans Cypriot")
    (set-fontset-font "fontset-default" 'deseret "Noto Sans Deseret")
    (set-fontset-font "fontset-default" 'devanagari "Noto Sans Devanagari")
    (set-fontset-font "fontset-default" 'egyptian "Noto Sans Egyptian Hieroglyphs Regular")
    (set-fontset-font "fontset-default" 'ethiopic "Noto Sans Ethiopic")
    (set-fontset-font "fontset-default" 'georgian "Noto Sans Georgian")
    (set-fontset-font "fontset-default" 'glagolitic "Noto Sans Glagolitic")
    (set-fontset-font "fontset-default" 'gothic "Noto Sans Gothic")
    (set-fontset-font "fontset-default" 'gujarati "Noto Sans Gujarati")
    (set-fontset-font "fontset-default" 'gurmukhi "Noto Sans Gurmukhi")
    (set-fontset-font "fontset-default" 'han "PingFang HK")
    (set-fontset-font "fontset-default" 'han "PingFang SC" nil 'append)
    (set-fontset-font "fontset-default" 'hangul "Noto Sans CJK KR Regular")
    (set-fontset-font "fontset-default" 'hanunoo "Noto Sans Hanunoo")
    (set-fontset-font "fontset-default" 'hebrew "Noto Sans Hebrew")
    (set-fontset-font "fontset-default" 'inscriptional-pahlavi "Noto Sans Inscriptional Pahlavi")
    (set-fontset-font "fontset-default" 'inscriptional-parthian "Noto Sans Inscriptional Parthian")
    (set-fontset-font "fontset-default" 'javanese "Noto Sans Javanese")
    (set-fontset-font "fontset-default" 'kaithi "Noto Sans Kaithi")
    (set-fontset-font "fontset-default" 'kana "Noto Sans CJK JP Regular")
    (set-fontset-font "fontset-default" 'kannada "Noto Sans Kannada")
    (set-fontset-font "fontset-default" 'kayah-li "Noto Sans Kayah Li")
    (set-fontset-font "fontset-default" 'kharoshthi "Noto Sans Kharoshthi")
    (set-fontset-font "fontset-default" 'khmer "Noto Sans Khmer")
    (set-fontset-font "fontset-default" 'lao "Noto Sans Lao")
    (set-fontset-font "fontset-default" 'lepcha "Noto Sans Lepcha")
    (set-fontset-font "fontset-default" 'limbu "Noto Sans Limbu")
    (set-fontset-font "fontset-default" 'linear-b "Noto Sans Linear B")
    (set-fontset-font "fontset-default" 'lisu "Noto Sans Lisu")
    (set-fontset-font "fontset-default" 'lycian "Noto Sans Lycian")
    (set-fontset-font "fontset-default" 'lydian "Noto Sans Lydian")
    (set-fontset-font "fontset-default" 'malayalam "Noto Sans Malayalam")
    (set-fontset-font "fontset-default" 'mandaic "Noto Sans Mandaic")
    (set-fontset-font "fontset-default" 'meetei-mayek "Noto Sans Meetei Mayek")
    (set-fontset-font "fontset-default" 'mongolian "Noto Sans Mongolian")
    (set-fontset-font "fontset-default" 'tai-lue "Noto Sans New Tai Lue Regular")
    (set-fontset-font "fontset-default" 'nko "Noto Sans NKo Regular")
    (set-fontset-font "fontset-default" 'ogham "Noto Sans Ogham")
    (set-fontset-font "fontset-default" 'ol-chiki "Noto Sans Ol Chiki")
    (set-fontset-font "fontset-default" 'old-italic "Noto Sans Old Italic Regular")
    (set-fontset-font "fontset-default" 'old-persian "Noto Sans Old Persian Regular")
    (set-fontset-font "fontset-default" 'old-south-arabian "Noto Sans Old South Arabian Regular")
    (set-fontset-font "fontset-default" 'old-turkic "Noto Sans Old Turkic")
    (set-fontset-font "fontset-default" 'oriya "Noto Sans Oriya")
    (set-fontset-font "fontset-default" 'osage "Noto Sans Osage")
    (set-fontset-font "fontset-default" 'osmanya "Noto Sans Osmanya")
    (set-fontset-font "fontset-default" 'phags-pa "Noto Sans Phags Pa")
    (set-fontset-font "fontset-default" 'phoenician "Noto Sans Phoenician")
    (set-fontset-font "fontset-default" 'rejang "Noto Sans Rejang")
    (set-fontset-font "fontset-default" 'runic "Noto Sans Runic")
    (set-fontset-font "fontset-default" 'samaritan "Noto Sans Samaritan")
    (set-fontset-font "fontset-default" 'saurashtra "Noto Sans Saurashtra")
    (set-fontset-font "fontset-default" 'shavian "Noto Sans Shavian")
    (set-fontset-font "fontset-default" 'sinhala "Noto Sans Sinhala")
    ;; (set-fontset-font "fontset-default" 'sinhala-archaic-number "Noto Sans Sinhala")
    (set-fontset-font "fontset-default" 'sundanese "Noto Sans Sundanese")
    (set-fontset-font "fontset-default" 'syloti-nagri "Noto Sans Syloti Nagri")
    ;;(set-fontset-font "fontset-default" 'syriac "Noto Sans Syriac Eastern")
    (set-fontset-font "fontset-default" 'syriac "Noto Sans Syriac Estrangela")
    ;;(set-fontset-font "fontset-default" 'syriac "Noto Sans Syriac Western")
    (set-fontset-font "fontset-default" 'tagalog "Noto Sans Tagalog")
    (set-fontset-font "fontset-default" 'tagbanwa "Noto Sans Tagbanwa")
    (set-fontset-font "fontset-default" 'tai-le "Noto Sans Tai Le")
    (set-fontset-font "fontset-default" 'tai-tham "Noto Sans Tai Tham")
    (set-fontset-font "fontset-default" 'tai-viet "Noto Sans Tai Viet")
    (set-fontset-font "fontset-default" 'tamil "Noto Sans Tamil")
    (set-fontset-font "fontset-default" 'telugu "Noto Sans Telugu")
    (set-fontset-font "fontset-default" 'thaana "Noto Sans Thaana")
    (set-fontset-font "fontset-default" 'thai "Noto Sans Thai")
    (set-fontset-font "fontset-default" 'tibetan "Noto Sans Tibetan")
    (set-fontset-font "fontset-default" 'tifinagh "Noto Sans Tifinagh")
    (set-fontset-font "fontset-default" 'ugaritic "Noto Sans Ugaritic")
    (set-fontset-font "fontset-default" 'vai "Noto Sans Vai")
    (set-fontset-font "fontset-default" 'yi "Noto Sans Yi")

    (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji")
    (set-fontset-font "fontset-default" 'symbol "Symbola" nil 'append)

    ;; Fix some chinese punctuation.
    ;; 、。
    (set-fontset-font "fontset-default" '(#x3001 . #x3017) "PingFang HK")
    ;; ，
    (set-fontset-font "fontset-default" '(#xff0c . #xff0c) "PingFang HK")

    ;; (set-fontset-font
    ;;  t 'han
    ;;  (font-spec
    ;;   :family "PingFang HK"
    ;;   :size 18
    ;;   :weight 'normal
    ;;   :width 'normal
    ;;   :slant 'normal))

    ;; (set-fontset-font
    ;;  t 'symbol
    ;;  (font-spec
    ;;   :family "Noto Color Emoji"
    ;;   :size 18
    ;;   :weight 'normal
    ;;   :width 'normal
    ;;   :slant 'normal))
    ))

(if *is-a-linux*
    (progn
      (setq use-default-font-for-symbols nil)
      (drsl/init-font)
      ;; Reduce blue light from screen.
      ;; (shell-command "redshift -O 3600K")
      (drsl/reset-screen-color)

      ;; Disable screen save.
      (shell-command "xset s off")
      (shell-command "xset -dpms")
      ))

(setq inhibit-startup-screen t)		; Disable startup screen.
(menu-bar-mode -1)                ; Disable menu bar.
(scroll-bar-mode -1)              ; Disable scroll bar.
(tool-bar-mode -1)                ; Disable tool bar.

(setq display-time-format "%b %e %H:%M")

;; This line prevent the mode line from showing useless CPU usage.
(setq display-time-load-average-threshold 10.0)
(display-time-mode t)                   ; Display time.

(column-number-mode t)
(blink-cursor-mode -1)                  ; Stop the stupid cursor from
                                        ; blinking
(global-visual-line-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(setq ring-bell-function 'ignore)	; No notification sound.
(global-auto-revert-mode t)       ; Auto revert. It doesn't work well with
                                        ; tramp.

;; (set-frame-parameter nil 'fullscreen 'fullboth)
(setq confirm-kill-processes nil)

;; Cleanup whitespace before saving.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Enable line number mode in programming modes.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Never use tab.
(setq-default indent-tabs-mode nil)

(setq use-short-answers t)

;; Custom directory for backup files.
(setq backup-directory-alist
      `((".*" . ,"~/.emacs-backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs-backups" t)))

;; Do not display these buffers.
(add-to-list 'display-buffer-alist '("\\*mbsync\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

;; I now want compilation to show up.
;; (add-to-list 'display-buffer-alist '("\\*compilation\\*"
;;                                      display-buffer-no-window
;;                                      ((allow-no-window . t))))
(add-to-list 'display-buffer-alist '("\\*compilation\\*"
                                     display-buffer-in-previous-window))

(add-to-list 'display-buffer-alist '("\\*Async Shell Command\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*rsync\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

(add-to-list 'display-buffer-alist '("\\*v2ray\\*"
                                     display-buffer-no-window
                                     ((allow-no-window . t))))

;; Password-Store always fuck up at displaying at a correct window.
(add-to-list 'display-buffer-alist '("\\*Password-Store\\*"
                                     display-buffer-same-window))

(add-to-list 'display-buffer-alist '("\\*shell\\*"
                                     display-buffer-same-window))

(add-to-list 'display-buffer-alist '("\\*w3m\\*"
                                     display-buffer-in-previous-window))

(add-to-list 'display-buffer-alist '("\\*sly-mrepl.*\\*"
                                     display-buffer-same-window))

;; This is extremely annoying when compiling native elisp.
(setq warning-minimum-level :emergency)

(if *is-a-mac*
    (progn
      (toggle-frame-maximized)
      (drsl/start-v2ray)))

(provide 'init-ui)
;;; init-ui.el ends here
