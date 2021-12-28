;;; shapeless-blog.el --- Minimalistic blogging with org mode. -*- lexical-binding: t -*-

;;; Commentary:

;; Shapeless Blog is a minimalistic blogging package in Emacs. It
;; works with a shapeless-blog server. Aim at providing a simple and
;; minimal blogging experience with Emacs.

;;; Code:

(require 'ox-shapelesshtml)

;; Early version of shapeless-blog only support pre-generated tokens.

(defcustom slblog-api-address nil
  "The api address for blog posting."
  :type 'string)

(defvar slblog-username nil
  "The user name of slblog.")

(defvar slblog-password nil
  "The user password of slblog.")

(defcustom slblog-token nil
  "The authentication token of slblog.
nil by default."
  :type 'string)

(defcustom slblog-export-backend 'shapelesshtml
  "The export backend of blog content."
  :type 'string)

;; From https://stackoverflow.com/questions/66574715/how-to-get-org-mode-file-title-and-other-file-level-properties-from-an-arbitra.
(defun slblog--org-get-value-of-key (DATA KEY)
  "Return the value associated with the KEY.
DATA is a string from `org-element-parse-buffer'
KEY is a string of the name of org element."
  (nth 1
       (assoc KEY
              (org-element-map DATA
                  '(keyword)
                (lambda (kwd)
                  (let ((x (cadr kwd)))
                    (list (plist-get x :key)
                          (plist-get x :value))))))))

(defun slblog--org-parse-greater-element-from-path (PATH)
  "Return a string containing org-data.
PATH is a string of file path."
  (with-temp-buffer
    (insert-file-contents PATH)
    (org-mode)
    (org-element-parse-buffer 'greater-element)))

(defun slblog--org-get-value-of-key-from-path (PATH KEY)
  "Return the value associated with the KEY.
PATH is a string of file path.
KEY is a string of the name of org element."
  (slblog--org-get-value-of-key (slblog--org-parse-greater-element-from-path PATH)
                                KEY))

(defun slblog--org-current-buffer-get-value-of-key (KEY)
  "Return the value of key from the current org buffer.
KEY is a string of a org element name.
e.g. TITLE, CATEGORY"
  (slblog--org-get-value-of-key (org-element-parse-buffer 'greater-element)
                                KEY))

(defun slblog--org-current-buffer-get-category ()
  "Return a list of string of the '#+CATEGORY:' field.

CATEGORY should originally be a csv string."
  ;; From csv to list.
  (mapcar 's-trim
          (split-string (slblog--org-current-buffer-get-value-of-key "CATEGORY")
                        ",")))

(defun slblog--org-current-buffer-get-title ()
  "Return a string of the '#+TITLE:' field."
  (slblog--org-current-buffer-get-value-of-key "TITLE"))

(defun slblog--org-current-buffer-get-date ()
  "Return a string of the '#+DATE:' field."
  (slblog--org-current-buffer-get-value-of-key "DATE"))

(defun slblog--org-current-buffer-get-update ()
  "Return a string of the '#+UPDATE:' field."
  (slblog--org-current-buffer-get-value-of-key "UPDATE"))

(defun slblog--org-current-buffer-get-id ()
  (slblog--org-current-buffer-get-value-of-key "ID"))

(defun slblog--html-body-content ()
  "Return a string of the body content in HTML using
'slblog-export-backend'."
  (org-export-as slblog-export-backend nil nil t))

(defun slblog--remove-timestamp-bracket (TIMESTAMP)
  "Remove the brackets of TIMESTAMP.

Return a TIMESTAMP string without brackets.

TIMESTAMP is a string."
  (replace-regexp-in-string "[]\[<>]" "" TIMESTAMP))

(defun slblog--update-date (TIMESTAMP)
  "Update the #+DATE: in org to TIMESTAMP

TIMESTAMP is a string."
  (replace-regexp "#\\+DATE:.*" (concat "#+DATE: " TIMESTAMP)
                  nil (point-min) (point-max)))

(defun slblog--update-time (TIMESTAMP)
  "Update the #+UPDATE: in org to TIMESTAMP.

TIMESTAMP is a string."
  (replace-regexp "#\\+UPDATE:.*" (concat "#+UPDATE: " TIMESTAMP)
                  nil (point-min) (point-max)))

(defun slblog--update-id (ID)
  "Update the #+ID: in org to ID.

ID is a string."
  (replace-regexp "#\\+ID:.*" (concat "#+ID: " (number-to-string ID))
                  nil (point-min) (point-max)))

(defun slblog--new-post-object (UPDATE)
  "Return a list of objects of the new post.

UPDATE is a timestamp string.
Note that it does not have an id."
  (list (cons "title" (slblog--org-current-buffer-get-title))
        (cons "created" (slblog--remove-timestamp-bracket
                         UPDATE))
        (cons "updated" (slblog--remove-timestamp-bracket
                         UPDATE))
        (cons "category" (slblog--org-current-buffer-get-category))
        (cons "content" (slblog--html-body-content))))

(defun slblog--old-post-object (UPDATE)
  "Return a list of objects for modifying an old post.

UPDATE is a timestamp string."
  (list (cons "id" (string-to-number (slblog--org-current-buffer-get-id)))
        (cons "title" (slblog--org-current-buffer-get-title))
        (cons "created" (slblog--remove-timestamp-bracket
                         (slblog--org-current-buffer-get-date)))
        (cons "updated" (slblog--remove-timestamp-bracket
                         UPDATE))
        (cons "category" (slblog--org-current-buffer-get-category))
        (cons "content" (slblog--html-body-content))))

(defun slblog--old-post-object-no-change ()
  "Return a list of objects for an old post."
  (list (cons "id" (string-to-number (slblog--org-current-buffer-get-id)))
        (cons "title" (slblog--org-current-buffer-get-title))
        (cons "created" (slblog--remove-timestamp-bracket
                         (slblog--org-current-buffer-get-date)))
        (cons "updated" (slblog--remove-timestamp-bracket
                         (slblog--org-current-buffer-get-update)))
        (cons "category" (slblog--org-current-buffer-get-category))
        (cons "content" (slblog--html-body-content))))

(defun slblog-post ()
  "Push the current buffer to the server, change the update time to now.

If 'id' is nil, create a new post.
Update the ID and UPDATE fields in current buffer.

If 'id' is non-nil, patch an old post.
Update the UPDATE field in current buffer."
  (interactive)
  (let ((id (slblog--org-current-buffer-get-id))
        (current-time (format-time-string "[%Y-%m-%d %H:%M]")))
    (if (string= id "nil")
        (let ((response (slblog--create-post-request current-time)))
          (if (string= (caar response)
                       "id")
              (progn
                (slblog--update-time current-time)
                (slblog--update-date current-time)
                (slblog--update-id (cdar response))
                (save-buffer)
                (message (format "created blog post %d" (cdar response))))
            (message (cdar response))))

      (let ((response (slblog--update-post-request id current-time)))
        (if (string= (caar response)
                     "id")
            (progn
              (slblog--update-time current-time)
              (save-buffer)
              (message "update success"))
          (message (cdar response)))))))

(defun slblog-post-no-change ()
  "Push the current buffer to the server, but do not change current buffer.

Can only be used in old post with a non-nil 'ID'."

  (interactive)
  (let ((id (slblog--org-current-buffer-get-id)))
    (if (string= id "nil")
        (message "current buffer has no ID")
      (let ((response (slblog--update-post-request-no-change id)))
        (if (string= (caar response)
                     "id")
            (message "update success")
          (message (cdar response)))))))

(defun slblog--post-address ()
  "The address of post CRUD."
  (concat slblog-api-address "post/"))

;; Forget about request.el
;; That adds a lot of complexity.
(defun slblog--create-post-request (UPDATE)
  "Make a POST request to the api server.

UPDATE is a timestamp string."
  (json-read-from-string
   (shell-command-to-string
    (format "curl -s -H 'Authorization: Bearer %s' -d %s %s"
            slblog-token
            (shell-quote-argument (json-encode (slblog--new-post-object UPDATE)))
            (slblog--post-address)))))

(defun slblog--update-post-request (ID UPDATE)
  "Make a PATCH request to the api server.

ID is a string of post id.
UPDATE is a timestamp string."
  (json-read-from-string
   (shell-command-to-string
    (format "curl -s -H 'Authorization: Bearer %s' -X PATCH -d %s %s"
            slblog-token
            (shell-quote-argument (json-encode (slblog--old-post-object UPDATE)))
            (concat (slblog--post-address) ID)))))

(defun slblog--update-post-request-no-change (ID)
  "Make a PATCH request to the api server.

ID is a string of post id."
  (json-read-from-string
   (shell-command-to-string
    (format "curl -s -H 'Authorization: Bearer %s' -X PATCH -d %s %s"
            slblog-token
            (shell-quote-argument (json-encode (slblog--old-post-object-no-change)))
            (concat (slblog--post-address) ID)))))

(defun slblog--authentication-address ()
  "The address for authentication."
  (concat slblog-api-address "authentication/"))

(defun slblog--get-token-request ()
  "Update 'slblog-token'.

Use 'slblog-username' and 'slblog-password' to authenticate."
  (json-read-from-string
   (shell-command-to-string
    (format "curl -s -d %s %s"
            (shell-quote-argument (json-encode (list (cons "username" slblog-username)
                                                     (cons "password" slblog-password))))
            (slblog--authentication-address)))))

(defun slblog-update-token ()
  "Update 'slblog-token'."
  (interactive)
  (let ((response (slblog--get-token-request)))
    (if (string= (caar response)
                 "token")
        (progn
          (setq slblog-token (cdar response))
          (message "token updated"))
      (message (cdar response)))))

(defcustom slblog--new-blog-template
     "#+TITLE:
#+DATE: nil
#+UPDATE: nil
#+CATEGORY:
#+ID: nil

# ## Blog post starts from here. ###

"
     "slblog template string."
     :type 'string)

(defun slblog-insert-new-blog-template ()
  "Insert a new blog template to current buffer."
  (interactive)
  (insert slblog--new-blog-template))

(provide 'shapeless-blog)
;;; shapeless-blog.el ends here
