;;; init-org.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains all org mode related things, including export
;; backend, minor modes and org roams.

;;; Code:

;; This will clone the latest version of org instead of the old
;; version shipped with Emacs. Sounds stupid, but having it handled by
;; straight prevent compatibility issues.
;; https://github.com/raxod502/straight.el#the-wrong-version-of-my-package-was-loaded

;; (straight-use-package '(org :type built-in))
(elpaca
 org
 (require 'org)
 (add-hook 'org-mode-hook #'auto-fill-mode)
 (add-hook 'org-mode-hook #'org-indent-mode)

 (setq org-return-follows-link t)
 (setq org-src-window-setup 'current-window)

 (setq org-time-stamp-custom-formats (cons "<%Y-%m-%d>" "<%Y-%m-%d %a %H:%M>"))
 (setq org-time-stamp-formats (cons "<%Y-%m-%d %H:%M>" "<%Y-%m-%d>"))

 (setq org-html-doctype "html5")

 (setq org-link-frame-setup
       '((vm . vm-visit-folder-other-frame)
         (vm-imap . vm-visit-imap-folder-other-frame)
         (gnus . org-gnus-no-new-news)
         (file . find-file)
         (wl . wl-other-frame)))

 (setq org-image-actual-width nil)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((dot . t)
    (shell . t))))

(elpaca-wait)

(elpaca
 valign
 (require 'valign))

;; (elpaca
;;  org-pdftools
;;  (require 'org-pdftools)
;;  (org-link-set-parameters "pdf-view"
;;                           :follow (lambda (path)
;;                                     (org-pdftools-open path))))

(elpaca
 (ox-shapelesshtml :host github :repo "drshapeless/ox-shapelesshtml")
 (require 'ox-shapelesshtml))

(elpaca
 org-roam
 (require 'org-roam)
 (keymap-global-set "C-c r i" #'org-roam-node-insert)
 (keymap-global-set "C-c r f" #'org-roam-node-find)

 (keymap-set org-mode-map "C-c r c" #'org-id-get-create)
 (keymap-set org-mode-map "C-c r r" #'org-roam-ref-add)
 (keymap-set org-mode-map "C-c r t" #'org-roam-tag-add)
 (keymap-set org-mode-map "C-c r a" #'org-roam-alias-add)

 (setq org-roam-directory "~/org-roam/"
       org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory)
       org-roam-v2-ack t)

 (org-roam-db-autosync-mode)
 ;; From https://jethrokuan.github.io/org-roam-guide/
 (setq org-roam-capture-templates
       '(("m" "main" plain "%?"
          :if-new (file+head "main/${slug}.org"
                             "#+title: ${title}\n")
          :immediate-finish t
          :unnarrowed t)
         ;; Review is for book, music, anime, manga.
         ("r" "review" plain "%?"
          :if-new
          (file+head "reviews/${slug}.org" "#+title: ${title}\n#+date: nil\n#+update: nil\n#+id: nil\n#+filetags: :review:\n")
          :immediate-finish t
          :unnarrowed t)
         ;; Article is for myself.
         ("a" "article" plain "%?"
          :if-new (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
          :immediate-finish t
          :unnarrowed t)
         ;; Blog is for publishing on personal blog.
         ("b" "blog" plain "%?"
          :if-new (file+head "blog/${slug}.org"
                             "#+title: ${title}\n#+filetags: :draft:\n#+date: nil\n#+update: nil\n\n")
          :immediate-finish t
          :unnarrowed t)))

 (setq org-capture-templates
       '(;; Slipbox for immediate thoughts.
         ("s" "Slipbox" entry (file "~/org-roam/inbox.org")
          "* %?\n")))

 (defun drsl/org-capture-slipbox ()
   (interactive)
   (org-capture nil "s"))

 (defun drsl/tag-new-node-as-draft ()
   (org-roam-tag-add '("draft")))
 (add-hook 'org-roam-capture-new-node-hook #'drsl/tag-new-node-as-draft)

 (cl-defmethod org-roam-node-type ((node org-roam-node))
   "Return the TYPE of NODE."
   (condition-case nil
       (file-name-nondirectory
        (directory-file-name
         (file-name-directory
          (file-relative-name (org-roam-node-file node) org-roam-directory))))
     (error "")))

 (setq org-roam-node-display-template
       (concat "${type:10} ${title:*} " (propertize "${tags:*}" 'face 'org-tag)))
 )

;; This package is for inline css while exporting source code block.
(elpaca
 htmlize
 (require 'htmlize))

(elpaca
 org-present
 (require 'org-present))

(elpaca
 ob-restclient
 (require 'ob-restclient))

;; This has to be after org.
(require 'org-crypt)
(org-crypt-use-before-save-magic)

(provide 'init-org)
;;; init-org.el ends here
