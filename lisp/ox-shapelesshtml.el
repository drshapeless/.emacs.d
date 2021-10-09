;;; ox-shapelesshtml.el --- shapeless HTML export backend.
;;; Commentary:

;; This is a html export backend derived from ox-slimhtml.
;; It adds code, and fixes unwanted newline character in Chinese
;; paragraph.

;;; Code:

(require 'ox-html)
(require 'ox-slimhtml)
(require 'cl-lib)

(org-export-define-derived-backend 'shapelesshtml 'slimhtml
  :translate-alist '((code . ox-shapelesshtml-code)
                     (paragraph . ox-shapelesshtml-paragraph)
                     (plain-list . ox-shapelesshtml-plain-list)))

;; code
;; ~code~        # <code>code</code>

(defun ox-shapelesshtml-code (code contents info)
  "Transcode CODE string from Org to HTML.
CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value code))))
    (when contents (format "<code>%s</code>" contents))))

(defun ox-shapelesshtml--remove-newline (string)
  "Remove unwanted newline char in STRING.

In Chinese, we don't want an extra space after a normal
character."
  (string-join (mapcar
                (lambda (str)
                  (if (string-match-p "[0-9a-z,.?!<>\"']\\'" str)
                      (concat str "\n")
                    str))
                (split-string string "\n"))))

(defun ox-shapelesshtml-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.

CONTENTS is the contents of the paragraph.
INFO is a plist holding contextual information."
  (when contents
    (if (or (ox-slimhtml--immediate-child-of-p paragraph 'item)
            (ox-slimhtml--immediate-child-of-p paragraph 'special-block))
        contents
      (if (ox-slimhtml--has-immediate-child-of-p paragraph 'link)
          (format "<p>%s</p>" (ox-shapelesshtml--remove-newline contents))
        (format "<p%s>%s</p>" (ox-slimhtml--attr paragraph) (ox-shapelesshtml--remove-newline contents))))))

(defun ox-shapelesshtml-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST string from Org to HTML.

CONTENTS is the contents of the list element.
INFO is a plist holding contextual information."
  (when contents
    (let ((type (cl-case (org-element-property :type plain-list)
                  (ordered "ol")
                  (unordered "ul")
                  (descriptive "dl"))))
      (format "<%s%s>%s</%s>" type (ox-slimhtml--attr plain-list) (ox-shapelesshtml--remove-newline contents) type))))

;;;###autoload
(defun ox-shapelesshtml-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a SHAPELESSHTML buffer.

Export as `org-html-export-as-html' does, with slimhtml
org-export-backend.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org SHAPELESSHTML export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'shapelesshtml "*Org SHAPELESSHTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun ox-shapelesshtml-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML file.

Export as `org-html-export-as-html' does, with shapelesshtml
org-export-backend.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
                                    org-html-extension
                                    "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'shapelesshtml file
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-shapelesshtml)
;;; ox-shapelesshtml.el ends here
