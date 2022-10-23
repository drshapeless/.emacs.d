;;; init-input-method.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Currently is all about Chinese.

;; Use cangjie5 package.

;;; Code:

(require 'cangjie5)

(setq default-input-method "cangjie5")

;; Extra characters for the original tsangchi input method. Deprecated.
;; (with-temp-buffer
;;   (activate-input-method "chinese-b5-tsangchi")
;;   (let ((quail-current-package (assoc "chinese-b5-tsangchi" quail-package-alist)))
;;     (quail-define-rules ((append . t))
;;                         ("ekha"  "潴")
;;                         ("buang" "瞤")
;;                         ("sewi"  "屬")
;;                         ("thvp"  "芪")
;;                         ("ehxe"  "溲")
;;                         ("cpi"   "鈎")
;;                         ("fhje"  "煅")
;;                         ("hafmu" "㿠")
;;                         ("tpkp"  "葱")
;;                         ("ynai"  "逸")
;;                         ("tqbu"  "着")
;;                         ("vmfht" "彜")
;;                         ("sehq"  "犀")
;;                         ("tnai"  "菟")
;;                         ("idd"   "麻")
;;                         ("tybd"  "藁")
;;                         ("bbsmh" "髎")
;;                         ("bbwlb" "髃")
;;                         ("iogb"  "膺")
;;                         ("kinl"  "郄")
;;                         ("yrytp" "譩")
;;                         ("rmwyi" "𧿹")
;;                         ("ufcb"  "嵴")
;;                         ("bbhpm" "骶")
;;                         ("buymp" "眦")
;;                         ("yylc"  "迹")
;;                         ("rhhe"  "嚟")
;;                         ("cbtu"  "鋼")
;;                         ("punl"  "邨")
;;                         ("rkm"   "咗")
;;                         )))

;; Add back some cangjie 3 characters back into the pool.
(with-temp-buffer
  (activate-input-method "cangjie5")
  (let ((quail-current-package (assoc "cangjie5" quail-package-alist)))
    (quail-define-rules ((append . t))
                        ("ohs"   "作")
                        ("lmyyy" "非")
                        ("igp"   "應")
                        )))

;; Input chinese punctuations with ease.
(require 'shapeless-chinese)
(defun drsl/toggle-shapeless-chinese ()
  (interactive)
  (if (equal shapeless-chinese-mode t)
      (setq shapeless-chinese-mode nil)
    (setq shapeless-chinese-mode t)))
(defun drsl/toggle-input-and-shapeless-chinese ()
  (interactive)
  (toggle-input-method)
  (drsl/toggle-shapeless-chinese))

(keymap-global-set "H-<SPC>" #'toggle-input-method)

(provide 'init-input-method)
;;; init-input-method.el ends here
