;;; init-input-method.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(setq default-input-method "chinese-b5-tsangchi")

;; Extra characters.
(with-temp-buffer
  (activate-input-method "chinese-b5-tsangchi")
  (let ((quail-current-package (assoc "chinese-b5-tsangchi" quail-package-alist)))
    (quail-define-rules ((append . t))
                        ("ekha"  "潴")
                        ("buang" "瞤")
                        ("sewi"  "屬")
                        ("thvp"  "芪")
                        ("ehxe"  "溲")
                        ("cpi"   "鈎")
                        ("fhje"  "煅")
                        ("hafmu" "㿠")
                        ("tpkp"  "葱")
                        ("ynai"  "逸")
                        ("tqbu"  "着")
                        ("vmfht" "彜")
                        ("sehq"  "犀")
                        ("tnai"  "菟")
                        ("idd"   "麻")
                        ("tybd"  "藁")
                        ("bbsmh" "髎")
                        ("bbwlb" "髃")
                        ("iogb"  "膺")
                        ("kinl"  "郄")
                        ("yrytp" "譩")
                        ("rmwyi" "𧿹")
                        ("ufcb"  "嵴")
                        ("bbhpm" "骶")
                        ("buymp" "眦")
                        ("yylc"  "迹")
                        ("rhhe"  "嚟")
                        ("cbtu"  "鋼")
                        )))

(provide 'init-input-method)
;;; init-input-method.el ends here