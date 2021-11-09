;;; init-org.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

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

  (setq org-time-stamp-custom-formats (cons "%Y-%m-%d" "%Y-%m-%d %a %H:%M"))
  (setq org-time-stamp-formats (cons "<%Y-%m-%d>" "<%Y-%m-%d %H:%M>"))

  (setq org-html-doctype "html5")
  )

(leaf valign
  :after org
  :hook org-mode)

(leaf org-pdftools
  :after pdf-tools org
  :config
  (org-link-set-parameters "pdf-view"
                           :follow (lambda (path)
                                     (org-pdftools-open path))))

(leaf ox-slimhtml)

(leaf ox-shapelesshtml
  :after ox-slimhtml org
  :straight nil
  :require t
  :config
  (setq org-publish-project-alist
      '(
        ("blog"
         :base-directory "~/website/org/published/blog"
         :publishing-directory "~/website/drshapeless/blog"
         :publishing-function ox-shapelesshtml-publish-to-html)
        ("blog_zh"
         :base-directory "~/website/org/published/blog_zh"
         :publishing-directory "~/website/drshapeless/blog"
         :publishing-function ox-shapelesshtml-publish-to-html)
        ("main-site"
         :base-directory "~/website/org/published/main-site"
         :publishing-directory "~/website/drshapeless/main-site"
         :publishing-function ox-shapelesshtml-publish-to-html)
        ("main-site_zh"
         :base-directory "~/website/org/published/main-site_zh"
         :publishing-directory "~/website/drshapeless/main-site"
         :publishing-function ox-shapelesshtml-publish-to-html)
        )))

(leaf org-roam
  :after org
  :require t
  :bind
  (:org-mode-map
   ("C-c r c" . org-id-get-create)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r f" . org-roam-node-find))
  :config
  (setq org-roam-directory "~/org-roam/"
        org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory)
        org-roam-v2-ack t)
  (org-roam-db-autosync-mode))

(leaf htmlize)

(provide 'init-org)
;;; init-org.el ends here
