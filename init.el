;;; init.el --- drshapeless emacs init. -*- lexical-binding: t -*-
;;; Commentary:

;; The structure of the init file is inspired by Steve Purcell's Emacs
;; configuration.

;;; Code:

;; Personal information.
(setq user-full-name "Jacky Li")
(setq user-mail-address "drsl@drshapeless.com")

;; Load the "lisp" folder.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; The order of the following must not change.
(require 'init-straight)
(require 'init-leaf)

(require 'init-org)

;; Emacs built-in config.
;; Although Emacs ships a version of these packages, it is usually
;; better to clone the latest version via straight.
(require 'init-tramp)
(require 'init-theme)
(require 'init-dired)
(require 'init-input-method)
(require 'init-shell)
(require 'init-epa)
;; (require 'init-erc)
(require 'init-flymake)
(require 'init-ediff)
(require 'init-ibuffer)

;; Extra packages
(require 'init-blackout)
(require 'init-mood-line)
;; (require 'init-doom-modeline)
(require 'init-yasnippet)
(require 'init-vertico)
(require 'init-orderless)
(require 'init-savehist)
(require 'init-marginalia)
(require 'init-embark)
(require 'init-consult)
(require 'init-which-key)
(require 'init-helpful)
(require 'init-aggressive-indent)
(require 'init-ripgrep)
(require 'init-shrface)
(require 'init-inherit-org)
(require 'init-request)
(require 'init-framemove)
(require 'init-projectile)
(require 'init-flutter)
(require 'init-cape)
(require 'init-corfu)
(require 'init-shell-switcher)
(require 'init-tree-sitter)
(require 'init-rainbow)

;; Major modes.
(require 'init-markdown)
(require 'init-swift)
(require 'init-go)
(require 'init-rust)
(require 'init-nginx)
(require 'init-web)
(require 'init-calibredb)
(require 'init-nov)
(require 'init-hackernews)
(require 'init-yaml)
(require 'init-glsl)
(require 'init-hlsl)
(require 'init-dart)
(require 'init-wat)

(require 'init-eglot)
(require 'init-pdf)
(require 'init-emms)
(require 'init-w3m)
(require 'init-magit)
(require 'init-vterm)
(require 'init-notmuch)
(require 'init-json)
(require 'init-lilypond)
(require 'init-mentor)
(require 'init-pass)
;; (require 'init-slime)
(require 'init-sly)
(require 'init-geiser)

(if *is-a-linux*
    (progn
      ;; This is about smooth scrolling in Emacs.
      ;; For macos, emacs-mac has a even better scrolling.
      (if (version< emacs-version "29.0.50")
          ;; For older version, just use good-scroll.
          (require 'init-good-scroll)
        ;; pixel-scroll-precision-mode is a new feature in version 29.
        (require 'init-pixel-scroll-precision))
      (require 'init-exwm)
      )
  )
(require 'init-helpers)
(require 'init-ui)
(require 'init-keybinding)
(require 'init-shapeless-blog)

(server-start)

(message "Successfully loaded all configurations.")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "74b9e99a8682c86659b8ace1610c4556c4619e6ca812a37b32d2c5f844fdafca" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "22ce392ec78cd5e512169f8960edf5cbbad70e01d3ed0284ea62ab813d4ff250" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "84b14a0a41bb2728568d40c545280dbe7d6891221e7fbe7c2b1c54a3f5959289" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "22aa7150cc5563723c2ff2773a38b06dc7682a7c5482b9a45670dab096385750" "baf99d71d17780801f41c0be391641d3032aa8121c2c41f379d3ec643590e6f9" "71cbf5d7f6ecdb19b43466aef10622bacbf6f5fbd924de567ddd99c52ae25230" "e449dc2222ba43d28759ffbc3b770f7186f4f5c3f9732475709f02d876ccdc8e" "143f15a0a1260761363230fadb7af13f9c76ab44dc55af47c61c789b40728484" default))
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
