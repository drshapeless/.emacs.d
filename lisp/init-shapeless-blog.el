;;; init-shapeless-blog.el --- shapeless blog -*- lexical-binding: t -*-

;;; Commentary:

;; A minimalistic blogging tool.

;;; Code:

(elpaca
 (shapeless-blog :host github :repo "drshapeless/emacs-shapeless-blog")
 (require 'shapeless-blog)
 ;; This function prevent Emacs from asking password at restarting
 ;; computer.
 (defun drsl/shapeless-blog-update-token ()
   "Fetch the secret via pass before updating token."
   (interactive)
   (setq shapeless-blog-username "jacky")
   (setq shapeless-blog-password (password-store-get "blog.drshapeless.com"))
   (shapeless-blog-update-token))

 (setq shapeless-blog-api-url "https://blog.drshapeless.com/api")

 (keymap-set org-mode-map "C-c b p" #'shapeless-blog-create-or-update-post)
 (keymap-set org-mode-map "C-c b t" #'drsl/shapeless-blog-update-token)

 (defun drsl/publish-one-blog (FILE OUTDIR)
   (find-file FILE)
   (let ((filename (file-name-sans-extension (file-name-nondirectory FILE)))
         (TAGS (shapeless-blog--get-tags))
         (TITLE (shapeless-blog--get-title))
         (CREATE (shapeless-blog--get-create-date))
         (UPDATE (shapeless-blog--get-update-date)))
     (ox-shapelesshtml-export-as-html nil nil nil t)
     (beginning-of-buffer)
     (insert (concat
              "<h1 id=\"title\">" TITLE "</h1>"
              "<div>Tags: "
              (s-join " | "
                      (mapcar
                       (lambda (TAG)
                         (concat "<a href=\"/blog/tags/"
                                 TAG
                                 ".html\">"
                                 TAG
                                 "</a>"))
                       TAGS))
              "</div>"
              "<p>"
              "Create: " CREATE
              ", "
              "Update: " UPDATE
              "</p>"))
     (write-file (concat
                  OUTDIR
                  (s-replace
                   " " "-"
                   (downcase TITLE))
                  ".html"))
     (kill-buffer)
     (kill-buffer (file-name-nondirectory FILE))))

 (defun drsl/publish-blogs (DIR OUTDIR)
   (mapc
    (lambda (FILE)
      (drsl/publish-one-blog FILE OUTDIR))
    (directory-files DIR t ".org")))

 (defun drsl/publish-english-blogs ()
   "Publish all English blogs."
   (interactive)
   (let ((HOME (getenv "HOME")))
     (drsl/publish-blogs (concat HOME "/org-roam/blog/") (concat HOME "/website/web/blog/"))))

 (defun drsl/publish-current-blog (OUTDIR)
   "Publish current blog"
   (let ((TAGS (shapeless-blog--get-tags))
         (TITLE (shapeless-blog--get-title))
         (CREATE (shapeless-blog--get-create-date))
         (UPDATE (shapeless-blog--get-update-date)))
     (ox-shapelesshtml-export-as-html nil nil nil t)
     (beginning-of-buffer)
     (insert (concat
              "<h1 id=\"title\">" TITLE "</h1>"
              "<div>Tags: "
              (s-join " | "
                      (mapcar
                       (lambda (TAG)
                         (concat "<a href=\"/blog/tags/"
                                 TAG
                                 ".html\">"
                                 TAG
                                 "</a>"))
                       TAGS))
              "</div>"
              "<p>"
              "Create: " CREATE
              ", "
              "Update: " UPDATE
              "</p>"))
     (write-file (concat
                  OUTDIR
                  (s-replace
                   " " "-"
                   (downcase TITLE))
                  ".html"))
     (kill-buffer)))

 (defun drsl/publish-current-blog-as-english ()
   "Publish current blog as an English blog.

Set the create and update date to now."
   (interactive)
   (drsl/blog-set-create-date-to-now)
   (drsl/blog-set-update-date-to-now)
   (save-buffer)
   (drsl/publish-current-blog
    (concat (getenv "HOME")
            "/website/web/blog/")))

 (defun drsl/update-current-blog-as-english ()
   "Update current blog as an English blog.

The difference between this and
`drsl/publish-current-blog-as-english' is that it does not update
create date."
   (interactive)
   (drsl/blog-set-update-date-to-now)
   (save-buffer)
   (drsl/publish-current-blog
    (concat (getenv "HOME")
            "/website/web/blog/")))

 (defun drsl/blog-set-create-date-to-now ()
   "Update #+date into now"
   (interactive)
   (shapeless-blog--edit-create-date (format-time-string "%Y-%m-%d")))

 (defun drsl/blog-set-update-date-to-now ()
   "Update #+update into now"
   (interactive)
   (shapeless-blog--edit-update-date (format-time-string "%Y-%m-%d"))))

(provide 'init-shapeless-blog)
;;; init-shapeless-blog.el ends here
