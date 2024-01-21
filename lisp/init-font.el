;;; init-font.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Setting up font in Emacs.

;;; Code:

(defun drsl/init-font ()
  (progn
    ;; (set-face-attribute 'default nil :font "monaco")

    ;; Provided by Sebastian Urban
    ;; More information at https://idiocy.org/emacs-fonts-and-fontsets.html
    (set-fontset-font t 'adlam "Noto Sans Adlam")
    (set-fontset-font t 'anatolian "Noto Sans Anatolian Hieroglyphs")
    (set-fontset-font t 'arabic "Noto Sans Arabic")
    (set-fontset-font t 'aramaic "Noto Sans Imperial Aramaic Regular")
    (set-fontset-font t 'armenian "Noto Sans Armenian")
    (set-fontset-font t 'avestan "Noto Sans Avestan")
    (set-fontset-font t 'balinese "Noto Sans Balinese")
    (set-fontset-font t 'bamum "Noto Sans Bamum")
    (set-fontset-font t 'batak "Noto Sans Batak")
    (set-fontset-font t 'bengali "Noto Sans Bengali")
    (set-fontset-font t 'brahmi "Noto Sans Brahmi")
    (set-fontset-font t 'buginese "Noto Sans Buginese")
    (set-fontset-font t 'buhid "Noto Sans Buhid")
    (set-fontset-font t 'burmese "Noto Sans Myanmar")
    (set-fontset-font t 'canadian-aboriginal "Noto Sans Canadian Aboriginal")
    (set-fontset-font t 'carian "Noto Sans Carian")
    (set-fontset-font t 'chakma "Noto Sans Chakma")
    (set-fontset-font t 'cham "Noto Sans Cham")
    (set-fontset-font t 'cherokee "Noto Sans Cherokee")
    (set-fontset-font t 'cjk-misc "Noto Sans CJK SC Regular")
    (set-fontset-font t 'coptic "Noto Sans Coptic Regular")
    (set-fontset-font t 'cuneiform "Noto Sans Cuneiform")
    (set-fontset-font t 'cypriot-syllabary "Noto Sans Cypriot")
    (set-fontset-font t 'deseret "Noto Sans Deseret")
    (set-fontset-font t 'devanagari "Noto Sans Devanagari")
    (set-fontset-font t 'egyptian "Noto Sans Egyptian Hieroglyphs Regular")
    (set-fontset-font t 'ethiopic "Noto Sans Ethiopic")
    (set-fontset-font t 'georgian "Noto Sans Georgian")
    (set-fontset-font t 'glagolitic "Noto Sans Glagolitic")
    (set-fontset-font t 'gothic "Noto Sans Gothic")
    (set-fontset-font t 'gujarati "Noto Sans Gujarati")
    (set-fontset-font t 'gurmukhi "Noto Sans Gurmukhi")
    (set-fontset-font t 'han "Noto Sans CJK HK")
    (set-fontset-font t 'han "Noto Sans CJK TC Regular" nil 'append)
    ;; (set-fontset-font t 'han "PingFang HK")
    ;; (set-fontset-font t 'han "PingFang SC" nil 'append)
    (set-fontset-font t 'hangul "Noto Sans CJK KR Regular")
    (set-fontset-font t 'hanunoo "Noto Sans Hanunoo")
    (set-fontset-font t 'hebrew "Noto Sans Hebrew")
    (set-fontset-font t 'inscriptional-pahlavi "Noto Sans Inscriptional Pahlavi")
    (set-fontset-font t 'inscriptional-parthian "Noto Sans Inscriptional Parthian")
    (set-fontset-font t 'javanese "Noto Sans Javanese")
    (set-fontset-font t 'kaithi "Noto Sans Kaithi")
    (set-fontset-font t 'kana "Noto Sans CJK JP Regular")
    (set-fontset-font t 'kannada "Noto Sans Kannada")
    (set-fontset-font t 'kayah-li "Noto Sans Kayah Li")
    (set-fontset-font t 'kharoshthi "Noto Sans Kharoshthi")
    (set-fontset-font t 'khmer "Noto Sans Khmer")
    (set-fontset-font t 'lao "Noto Sans Lao")
    (set-fontset-font t 'lepcha "Noto Sans Lepcha")
    (set-fontset-font t 'limbu "Noto Sans Limbu")
    (set-fontset-font t 'linear-b "Noto Sans Linear B")
    (set-fontset-font t 'lisu "Noto Sans Lisu")
    (set-fontset-font t 'lycian "Noto Sans Lycian")
    (set-fontset-font t 'lydian "Noto Sans Lydian")
    (set-fontset-font t 'malayalam "Noto Sans Malayalam")
    (set-fontset-font t 'mandaic "Noto Sans Mandaic")
    (set-fontset-font t 'meetei-mayek "Noto Sans Meetei Mayek")
    (set-fontset-font t 'mongolian "Noto Sans Mongolian")
    (set-fontset-font t 'tai-lue "Noto Sans New Tai Lue Regular")
    (set-fontset-font t 'nko "Noto Sans NKo Regular")
    (set-fontset-font t 'ogham "Noto Sans Ogham")
    (set-fontset-font t 'ol-chiki "Noto Sans Ol Chiki")
    (set-fontset-font t 'old-italic "Noto Sans Old Italic Regular")
    (set-fontset-font t 'old-persian "Noto Sans Old Persian Regular")
    (set-fontset-font t 'old-south-arabian "Noto Sans Old South Arabian Regular")
    (set-fontset-font t 'old-turkic "Noto Sans Old Turkic")
    (set-fontset-font t 'oriya "Noto Sans Oriya")
    (set-fontset-font t 'osage "Noto Sans Osage")
    (set-fontset-font t 'osmanya "Noto Sans Osmanya")
    (set-fontset-font t 'phags-pa "Noto Sans Phags Pa")
    (set-fontset-font t 'phoenician "Noto Sans Phoenician")
    (set-fontset-font t 'rejang "Noto Sans Rejang")
    (set-fontset-font t 'runic "Noto Sans Runic")
    (set-fontset-font t 'samaritan "Noto Sans Samaritan")
    (set-fontset-font t 'saurashtra "Noto Sans Saurashtra")
    (set-fontset-font t 'shavian "Noto Sans Shavian")
    (set-fontset-font t 'sinhala "Noto Sans Sinhala")
    ;; (set-fontset-font t 'sinhala-archaic-number "Noto Sans Sinhala")
    (set-fontset-font t 'sundanese "Noto Sans Sundanese")
    (set-fontset-font t 'syloti-nagri "Noto Sans Syloti Nagri")
    ;;(set-fontset-font t 'syriac "Noto Sans Syriac Eastern")
    (set-fontset-font t 'syriac "Noto Sans Syriac Estrangela")
    ;;(set-fontset-font t 'syriac "Noto Sans Syriac Western")
    (set-fontset-font t 'tagalog "Noto Sans Tagalog")
    (set-fontset-font t 'tagbanwa "Noto Sans Tagbanwa")
    (set-fontset-font t 'tai-le "Noto Sans Tai Le")
    (set-fontset-font t 'tai-tham "Noto Sans Tai Tham")
    (set-fontset-font t 'tai-viet "Noto Sans Tai Viet")
    (set-fontset-font t 'tamil "Noto Sans Tamil")
    (set-fontset-font t 'telugu "Noto Sans Telugu")
    (set-fontset-font t 'thaana "Noto Sans Thaana")
    (set-fontset-font t 'thai "Noto Sans Thai")
    (set-fontset-font t 'tibetan "Noto Sans Tibetan")
    (set-fontset-font t 'tifinagh "Noto Sans Tifinagh")
    (set-fontset-font t 'ugaritic "Noto Sans Ugaritic")
    (set-fontset-font t 'vai "Noto Sans Vai")
    (set-fontset-font t 'yi "Noto Sans Yi")

    (set-fontset-font t 'symbol "Noto Color Emoji")
    (set-fontset-font t 'symbol "Symbola" nil 'append)

    ;; Fix some chinese punctuation.
    ;; 、。
    (set-fontset-font t '(#x3001 . #x3017) "Noto Sans CJK HK")
    ;; ，
    (set-fontset-font t '(#xff0c . #xff0c) "Noto Sans CJK HK")
    ))

(setq use-default-font-for-symbols nil)

(when *is-a-linux*
  (drsl/init-font))


(create-default-fontset)

(defun drsl/set-font ()
  (when *is-a-mac*
    (add-to-list 'default-frame-alist
                 '(font . "monaco-13")))
  (when *is-a-linux*
    (add-to-list 'default-frame-alist
                 '(font . "Hack-13"))))

(drsl/set-font)

;; Set font size after loading a theme.
(setq after-load-theme-hook #'drsl/set-font)

(provide 'init-font)
;;; init-font.el ends here
