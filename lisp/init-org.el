;;; init-org.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains all org mode related things, including export
;; backend, minor modes and org roams.

;;; Code:

;; This will clone the latest version of org instead of the old
;; version shipped with Emacs. Sounds stupid, but having it handled by
;; straight prevent compatibility issues.
;; https://github.com/raxod502/straight.el#the-wrong-version-of-my-package-was-loaded

(leaf org
  :require t
  :hook
  (org-mode-hook . auto-fill-mode)
  (org-mode-hook . org-indent-mode)
  :config
  (setq org-return-follows-link t)
  (setq org-src-window-setup 'current-window)

  (setq org-time-stamp-custom-formats (cons "<%Y-%m-%d>" "<%Y-%m-%d %a %H:%M>"))
  (setq org-time-stamp-formats (cons "<%Y-%m-%d %H:%M>" "<%Y-%m-%d>"))

  (setq org-html-doctype "html5")
  )

;; `require' does not do shit, use load. Straight has take care of the load path.

;; Manually loading these will fix some void definitions of functions,
;; e.g. `org-replace-buffer-contents' and `org-file-name-concat'.
;; (load "org-macs")
;; (load "org-compat")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (shell . t)))

(leaf valign
  ;; :hook org-mode-hook
  )

(leaf org-pdftools
  :after pdf-tools org
  :config
  (org-link-set-parameters "pdf-view"
                           :follow (lambda (path)
                                     (org-pdftools-open path))))

(leaf ox-slimhtml
  :require t)

(leaf ox-shapelesshtml
  :after ox-slimhtml org
  :straight nil
  :require t
  :config
  ;; (setq org-publish-project-alist
  ;;     '(
  ;;       ("blog"
  ;;        :base-directory "~/website/org/published/blog"
  ;;        :publishing-directory "~/website/drshapeless/blog"
  ;;        :publishing-function ox-shapelesshtml-publish-to-html)
  ;;       ("blog_zh"
  ;;        :base-directory "~/website/org/published/blog_zh"
  ;;        :publishing-directory "~/website/drshapeless/blog"
  ;;        :publishing-function ox-shapelesshtml-publish-to-html)
  ;;       ("main-site"
  ;;        :base-directory "~/website/org/published/main-site"
  ;;        :publishing-directory "~/website/drshapeless/main-site"
  ;;        :publishing-function ox-shapelesshtml-publish-to-html)
  ;;       ("main-site_zh"
  ;;        :base-directory "~/website/org/published/main-site_zh"
  ;;        :publishing-directory "~/website/drshapeless/main-site"
  ;;        :publishing-function ox-shapelesshtml-publish-to-html)
  ;;       ))
  )

(leaf org-roam
  :after org
  :require t
  :bind
  ("C-c r i" . org-roam-node-insert)
  ("C-c r f" . org-roam-node-find)
  (:org-mode-map
   ("C-c r c" . org-id-get-create)
   ("C-c r r" . org-roam-ref-add)
   ("C-c r t" . org-roam-tag-add)
   ("C-c r a" . org-roam-alias-add)
   )
  :config
  (setq org-roam-directory "~/org-roam/"
        org-roam-db-location (expand-file-name "org-roam.db" user-emacs-directory)
        org-roam-v2-ack t)

  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(("m" "main" plain "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "references/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ;; Article is for myself.
        ("a" "article" plain "%?"
         :if-new (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:draft:\n")
         :immediate-finish t
         :unnarrowed t)
        ;; Blog is for publishing on personal blog.
        ("b" "blog" plain "%?"
         :if-new (file+head "blog/${slug}.org"
                            "#+title: ${title}\n#+date: nil\n#+update: nil\n#+id: nil\n")
         :immediate-finish t
         :unnarrowed t)))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;; This package is for inline css while exporting source code block.
(leaf htmlize
  :require t)

(leaf org-present
  :after org
  :require t
  :hook
  (org-present-mode-hook . (lambda ()
                             (org-present-big)
                             (org-display-inline-images)
                             (org-present-hide-cursor)
                             (org-present-read-only)))
  (org-present-mode-quit-hook . (lambda ()
                                  (org-present-small)
                                  (org-remove-inline-images)
                                  (org-present-show-cursor)
                                  (org-present-read-write))))

(provide 'init-org)
;;; init-org.el ends here
