;;; init-org.el ---
;;; Commentary:

;;

;;; Code:

(require 'org)
;; This is a pixel perfect alignment for org table.
(add-hook 'org-mode-hook 'valign-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
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

(setq org-publish-project-alist
      '(
        ("blog"
         :base-directory "~/website/org/published/blog"
         :publishing-directory "~/website/drshapeless/blog"
         :publishing-function ox-slimhtml-publish-to-html)
        ("blog_zh"
         :base-directory "~/website/org/published/blog_zh"
         :publishing-directory "~/website/drshapeless/blog"
         :publishing-function ox-slimhtml-publish-to-html)
        ("main-site"
         :base-directory "~/website/org/published/main-site"
         :publishing-directory "~/website/drshapeless/main-site"
         :publishing-function ox-slimhtml-publish-to-html)
        ("main-site_zh"
         :base-directory "~/website/org/published/main-site_zh"
         :publishing-directory "~/website/drshapeless/main-site"
         :publishing-function ox-slimhtml-publish-to-html)
        ))

(use-package valign)
(diminish 'valign-mode)

(use-package org-pdftools)

(provide 'init-org)
;;; init-org.el ends here
