;;; init-org.el ---
;;; Commentary:

;; Migrated init-org-roam into init-org.

;;; Code:

(require 'org)
;; This is a pixel perfect alignment for org table.
(add-hook 'org-mode-hook 'valign-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(org-link-set-parameters "pdf-view"
                         :follow (lambda (path)
                                   (org-pdftools-open path)))

(setq org-return-follows-link t)
(setq org-src-window-setup 'current-window)

(setq org-time-stamp-custom-formats (cons "%Y-%m-%d" "%Y-%m-%d %a %H:%M"))
(setq org-time-stamp-formats (cons "<%Y-%m-%d>" "<%Y-%m-%d %H:%M>"))

(setq org-html-doctype "html5")

(require 'ox-publish)
(use-package ox-slimhtml)
(require 'ox-shapelesshtml)

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
        ))

(use-package valign)
(diminish 'valign-mode)

(use-package org-pdftools)

(use-package org-roam
  :bind (:map org-mode-map
              (("C-c r c" . org-id-get-create)
               ("C-c r i" . org-roam-node-insert)
               ("C-c r f" . org-roam-node-find))))

(setq org-roam-directory "~/org-roam")
(setq org-roam-v2-ack t)
(org-roam-db-autosync-mode)

(use-package htmlize)

(provide 'init-org)
;;; init-org.el ends here
