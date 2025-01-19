;;; init-go.el --- Major mode for golang -*- lexical-binding: t -*-

;;; Commentary:

;; The code is mainly from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md.

;; Don't ever use space over tab in golang, golang wants tab.

;;; Code:

(elpaca
    go-mode
  (require 'go-mode)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'go-ts-mode-hook (lambda () (setq tab-width 4)))
  (setq go-ts-mode-indent-offset 4))

(elpaca
    templ-ts-mode
  (require 'templ-ts-mode)
  (add-hook 'templ-ts-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'templ-ts-mode-hook #'rustywind-format-on-save)

  (defun drsl/is-class-attr ()
    (if (treesit-available-p)
        (let ((mynode (treesit-node-parent
                       (treesit-node-parent
                        (treesit-node-at (point))))))
          (and (string= (treesit-node-type mynode) "attribute")
               (string= (treesit-node-text (treesit-node-child mynode 0))
                        "class")))
      (let ((pos (save-excursion
                   (search-backward "="
                                    (line-beginning-position)
                                    t))))
        (when pos
          (and (string= "class" (buffer-substring (- pos 5) pos))
               (nth 3 (syntax-ppss)))))))

  (defun drsl/bounds-of-keyword ()
    (if (or (char-equal (char-before) ?\s)
            (char-equal (char-before) ?\"))
        nil
      (let ((START (save-excursion
                     (re-search-backward
                      "[<\"\s\t:]"
                      (line-beginning-position)
                      t)))
            (END (save-excursion
                   (re-search-forward
                    "[>\s\"]"
                    (line-end-position)
                    t))))
        (cond ((and START END)
               (cons (1+ START)
                     (1- END)))
              ((and START (not END))
               (cons (1+ START)
                     (line-end-position)))
              ((and (not START) END)
               (cons (line-beginning-position) (1- END)))
              (t (cons (line-beginning-position) (line-end-position)))))))

  (defcustom drsl/tailwind-css-keyword-file
    (expand-file-name "dict/tailwindcss_dict.txt" user-emacs-directory)
    "tailwindcss keyword file path."
    :type 'string)

  (defun drsl/tailwind-css-dict-list (input)
    "Return all words from `drsl/tailwind-css-keyword-file' matching INPUT."
    (unless (equal input "")
      (let* ((inhibit-message t)
             (message-log-max nil)
             (default-directory
              (if (and (not (file-remote-p default-directory))
                       (file-directory-p default-directory))
                  default-directory
                user-emacs-directory))
             (files (mapcar #'expand-file-name
                            (ensure-list
                             drsl/tailwind-css-keyword-file)))
             (words
              (apply #'process-lines-ignore-status
                     "grep"
                     (concat "-Fh"
                             (and (cape--case-fold-p cape-dict-case-fold) "i")
                             (and cape-dict-limit (format "m%d" cape-dict-limit)))
                     input files)))
        (cons
         (apply-partially
          (if (and cape-dict-limit (length= words cape-dict-limit))
              #'equal #'string-search)
          input)
         (cape--case-replace-list cape-dict-case-replace input words)))))

  (defun drsl/templ-tailwind-cape-dict ()
    (when (drsl/is-class-attr)
      (pcase-let ((`(,beg . ,end) (drsl/bounds-of-keyword)))
        `(,beg ,end
               ,(completion-table-case-fold
                 (cape--dynamic-table beg end #'drsl/tailwind-css-dict-list)
                 (not (cape--case-fold-p cape-dict-case-fold)))
               ,@cape--dict-properties))))

  (defvar drsl/templ-ts-mode-tag-list
    '("a" "abbr" "address" "area" "article" "aside" "audio" "b"
      "base" "bdi" "bdo" "blockquote" "body" "br" "button" "canvas"
      "caption" "cite" "code" "col" "colgroup" "data" "datalist"
      "dd" "del" "details" "dfn" "dialog" "div" "dl" "dt" "em"
      "embed" "fieldset" "figcaption" "figure" "footer" "form" "h1"
      "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html"
      "i" "iframe" "img" "input" "ins" "kbd" "label" "legend" "li"
      "link" "main" "map" "mark" "math" "menu" "meta" "meter" "nav"
      "noscript" "object" "ol" "optgroup" "option" "output" "p"
      "picture" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp"
      "script" "search" "section" "select" "slot" "small" "source"
      "span" "strong" "style" "sub" "summary" "sup" "svg" "table"
      "tbody" "td" "template" "textarea" "tfoot" "th" "thead" "time"
      "title" "tr" "track" "u" "ul" "var" "video" "wbr")
    "HTML tags used for completion.

Steal from `web-mode'.")

  (defvar drsl/templ-ts-mode-attribute-list
    '("accept" "accesskey" "action" "alt" "async" "autocomplete" "autofocus"
      "autoplay" "charset" "checked" "cite" "class" "cols" "colspan" "content"
      "contenteditable" "controls" "coords" "data" "datetime" "default" "defer"
      "dir" "dirname" "disabled" "download" "draggable" "enctype" "for" "form"
      "formaction" "headers" "height" "hidden" "high" "href" "hreflang" "http"
      "id" "ismap" "kind" "label" "lang" "list" "loop" "low" "max" "maxlength"
      "media" "method" "min" "multiple" "muted" "name" "novalidate" "onabort"
      "onafterprint" "onbeforeprint" "onbeforeunload" "onblur" "oncanplay"
      "oncanplaythrough" "onchange" "onclick" "oncontextmenu" "oncopy"
      "oncuechange" "oncut" "ondblclick" "ondrag" "ondragend" "ondragenter"
      "ondragleave" "ondragover" "ondragstart" "ondrop" "ondurationchange"
      "onemptied" "onended" "onerror" "onfocus" "onhashchange" "oninput"
      "oninvalid" "onkeydown" "onkeypress" "onkeyup" "onload" "onloadeddata"
      "onloadedmetadata" "onloadstart" "onmousedown" "onmousemove" "onmouseout"
      "onmouseover" "onmouseup" "onmousewheel" "onoffline" "ononline"
      "onpagehide" "onpageshow" "onpaste" "onpause" "onplay" "onplaying"
      "onpopstate" "onprogress" "onratechange" "onreset" "onresize" "onscroll"
      "onsearch" "onseeked" "onseeking" "onselect" "onstalled" "onstorage"
      "onsubmit" "onsuspend" "ontimeupdate" "ontoggle" "onunload"
      "onvolumechange" "onwaiting" "onwheel" "open" "optimum" "pattern"
      "placeholder" "poster" "preload" "readonly" "rel" "required" "reversed"
      "rows" "rowspan" "sandbox" "scope" "selected" "shape" "size" "sizes"
      "span" "spellcheck" "src" "srcdoc" "srclang" "srcset" "start" "step"
      "style" "tabindex" "target" "title" "translate" "type" "usemap" "value"
      "width" "wrap")
    "HTML attributes used for completion.

Steal from `web-mode'.")

  (defun drsl/templ-ts-mode-completion ()
    "templ-ts-mode completion function.

The built-in treesit is required."
    (cond (;; completing tag name, e.g. <d
           (let ((bounds (or (bounds-of-thing-at-point 'word)
                             (cons (point) (point)))))
             (when (char-equal (char-before (car bounds)) ?\<)
               (list (car bounds)
                     (cdr bounds)
                     drsl/templ-ts-mode-tag-list
                     :annotation-function (lambda (_) " HTML Tag")
                     :company-kind (lambda (_) 'text)
                     :exclude 'no))))

          (;; completing attribute name, e.g. <div c
           (or (string= (treesit-node-type (treesit-node-at (point)))
                        "attribute_name")
               (string= (treesit-node-type (treesit-node-at (point)))
                        ">")
               (string= (treesit-node-type (treesit-node-at (point)))
                        "/>"))
           (let ((bounds (bounds-of-thing-at-point 'word)))
             (when bounds
               (list (car bounds)
                     (cdr bounds)
                     drsl/templ-ts-mode-attribute-list
                     :annotation-function (lambda (_) " HTML Attr")
                     :company-kind (lambda (_) 'text)
                     :exclusive 'no))))
          (;; completing attribute value, e.g. <input type="text"
           (string= (treesit-node-type (treesit-node-parent
                                        (treesit-node-at (point))))
                    "quoted_attribute_value")
           (let ((words (drsl/get-html-value-list
                         (treesit-node-text
                          (treesit-node-prev-sibling
                           (treesit-node-parent (treesit-node-at (point)))
                           t)
                          t)))
                 (bounds (or (bounds-of-thing-at-point 'word)
                             (cons (point) (point)))))
             (when words
               (list (car bounds)
                     (cdr bounds)
                     words
                     :annotation-function (lambda (_) " HTML value")
                     :company-kind (lambda (_) 'text)
                     :exclusive 'no))))))

  (defvar drsl/htmx-attribute-list
    '("hx-get" "hx-post" "hx-on" "hx-push-url" "hx-select" "hx-select-oob"
      "hx-swap" "hx-swap-oob" "hx-target" "hx-trigger" "hx-vals" "hx-boost"
      "hx-confirm" "hx-delete" "hx-disable" "hx-disabled-elt" "hx-disinherit"
      "hx-encoding" "hx-ext" "hx-headers" "hx-history" "hx-history-elt"
      "hx-include" "hx-indicator" "hx-params" "hx-patch" "hx-preserve"
      "hx-prompt" "hx-put" "hx-replace-url" "hx-request" "hx-sync" "hx-validate")

    "Htmx attributes used for completion.")

  (defvar drsl/htmx-swap-keyword-list
    '("innerHTML" "outerHTML" "beforebegin" "afterbegin" "beforeend"
      "afterend" "delete" "none")
    "Keywords for hx-swap.")

  (defvar drsl/htmx-target-keyword-list
    '("this" "closest" "find" "next" "previous")
    "Keywords for hx-target.")

  (defvar drsl/html-input-type-list
    '("text" "password" "email" "url" "tel" "number" "range" "date" "time" "datetime-local" "month" "week" "color" "checkbox" "radio" "file" "hidden" "submit" "reset" "button")
    "Keywords for html input type.")

  (defun drsl/get-htmx-value-list (ATTR)
    "Return a list of htmx value.

ATTR is the attribute name.
Only support hx-swap, hx-swap-oob, hx-target."
    (cond ((string-prefix-p "hx-swap" ATTR)
           drsl/htmx-swap-keyword-list)
          ((string= ATTR "hx-target")
           drsl/htmx-target-keyword-list)))

  (defun drsl/get-html-value-list (ATTR)
    "Return a list of HTML value.

ATTR is the attribute name.
Only support type."
    (cond ((string= ATTR "type")
           drsl/html-input-type-list)))

  (defun drsl/templ-ts-mode-htmx-completion ()
    "templ-ts-mode completion for htmx.

Built-in treesit is required."
    (cond (;; completion of htmx attr name, e.g. <div hx-swap
           (or (string= (treesit-node-type (treesit-node-at (point)))
                        "attribute_name")
               (string= (treesit-node-type (treesit-node-at (point)))
                        ">"))
           ;; This mess if for the case when a - is typed.
           ;;
           ;; In the case of hx-swap*, where * is the pointer.
           ;;
           ;; Since word only includes swap, but symbol includes from
           ;; hx-swap... to the infinity, so just select the first of
           ;; symbol and last of word. But when a - is type, the
           ;; bounds of word returns nil, so just set it to the
           ;; `point'.
           ;;
           ;; TODO: This is an issue in the syntax table of
           ;; `templ-ts-mode'.
           ;;
           (let ((bounds (drsl/bounds-of-keyword)))
             (when bounds
               (list (car bounds)
                     (cdr bounds)
                     drsl/htmx-attribute-list
                     :annotation-function (lambda (_) " htmx attr")
                     :company-kind (lambda (_) 'text)
                     :exclusive 'no)))
           )
          (;; completion of some htmx value, e.g. <div hx-swap="innerHTML"
           (string= (treesit-node-type (treesit-node-parent
                                        (treesit-node-at (point))))
                    "quoted_attribute_value")
           (let ((words (drsl/get-htmx-value-list
                         (treesit-node-text
                          (treesit-node-prev-sibling
                           (treesit-node-parent (treesit-node-at (point)))
                           t)
                          t)))
                 (bounds (or (bounds-of-thing-at-point 'word)
                             (cons (point) (point)))))
             (when words
               (list (car bounds)
                     (cdr bounds)
                     words
                     :annotation-function (lambda (_) " htmx value")
                     :company-kind (lambda (_) 'text)
                     :exclusive 'no)
               ))
           )))

  (defun drsl/templ-ts-mode-insert-slash ()
    "Auto closing tag when inserting slash in `templ-ts-mode'"
    (interactive)
    (if (char-equal (char-before) ?\<)
        (let ((TAG (or (treesit-node-text
                        (treesit-node-child
                         (drsl/treesit-prev-sibling-until
                          (treesit-node-at (point))
                          (lambda (NODE)
                            (string= (treesit-node-type NODE)
                                     "tag_start")))
                         1)
                        t)

                       (when (or (drsl/treesit-next-sibling-until
                                  (treesit-node-parent (treesit-node-at (point)))
                                  (lambda (NODE)
                                    (string= (treesit-node-type NODE)
                                             "tag_end")))
                                 (string= (treesit-node-type
                                           (treesit-node-parent
                                            (treesit-node-at (point))))
                                          "ERROR"))
                         (treesit-node-text
                          (treesit-node-child
                           (drsl/treesit-prev-sibling-until
                            (treesit-node-parent (treesit-node-at (point)))
                            (lambda (NODE)
                              (string= (treesit-node-type NODE)
                                       "tag_start")))
                           1)
                          t))
                       )))
          (if TAG
              (progn (insert ?\/
                             TAG
                             ?\>)
                     (treesit-indent))
            (insert ?\/)))
      (insert ?\/)))

  (keymap-set templ-ts-mode-map "/" #'drsl/templ-ts-mode-insert-slash)

  (add-hook 'templ-ts-mode-hook
            (lambda ()
              (setq-local drsl/eglot-extra-completion-functions
                          (list
                           (cape-capf-super
                            #'drsl/templ-ts-mode-htmx-completion
                            #'drsl/templ-ts-mode-completion
                            )
                           #'drsl/templ-tailwind-cape-dict
                           )))))

(defun rustywind-format ()
  (interactive)
  (if (f-executable-p (executable-find "rustywind"))
      (let ((tmpfile (make-nearby-temp-file "rustywind" nil nil))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))

        (unwind-protect
            (save-restriction
              (widen)
              (write-region nil nil tmpfile)

              (let ((rustywind-args (list "--write" (file-local-name tmpfile))))
                (when (zerop (apply #'process-file (executable-find "rustywind") nil nil nil rustywind-args))
                  (insert-file-contents tmpfile nil nil nil t))))

          (delete-file tmpfile)))
    (error (format "Can't find rustywind"))))

(defun rustywind-format-on-save ()
  (add-hook 'before-save-hook #'rustywind-format nil t))

(defun drsl/treesit-prev-sibling-until (NODE PRED)
  "Find previous sibling until PRED is t.

PRED is a function which accept a NODE."
  ;; This is recursion.
  (when NODE
    (let ((sibling (treesit-node-prev-sibling NODE)))
      (when sibling
        (if (funcall PRED sibling)
            sibling
          (drsl/treesit-prev-sibling-until sibling PRED)))))
  )

(defun drsl/treesit-next-sibling-until (NODE PRED)
  "Find next sibling until PRED is t.

PRED is a function which accept a NODE."
  (when NODE
    (let ((sibling (treesit-node-next-sibling NODE)))
      (when sibling
        (if (funcall PRED sibling)
            sibling
          (drsl/treesit-next-sibling-until sibling PRED))))))

(defun drsl/go-db ()
  "Insert the snake_case version of current field "
  (interactive)
  (let ((word (string-inflection-underscore-function (current-word))))
    (end-of-line)
    (insert " `db:\"" word "\"`")))

(provide 'init-go)
;;; init-go.el ends here
