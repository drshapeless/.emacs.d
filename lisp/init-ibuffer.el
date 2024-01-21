;;; init-ibuffer.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Group buffers into categories. First by mode or functionality, then
;; by project. Projects without a .git would not be identified as a
;; project. To deal with this, initialize an empty git.

;;; Code:

(elpaca ibuffer-project
        (require 'ibuffer-project)

        (defvar drsl/ibuffer-filter-groups
          '(("shell" (or (mode . shell-mode)
                         (mode . vterm-mode)))
            ("mail" (or (mode . notmuch-show-mode)
                        (mode . notmuch-search-mode)
                        (mode . notmuch-hello-mode)
                        (name . "^\\*sent mail")
                        (name . "^\\*unsent mail")))
            ("Helpful" (or (mode . helpful-mode)
                           (mode . help-mode)))
            ("emacs" (or (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Warnings\\*$")
                         (name . "^\\*Quail Completions\\*$")
                         (name . "^\\*Async-native-compile-log\\*$")
                         (name . "^\\*Backtrace\\*$")
                         (name . "^\\*Native-compile-Log\\*$")
                         (name . "^\\*info\\*$")
                         (name . "^\\*straight-process\\*$")
                         (name . "^\\*v2ray\\*$")
                         (name . "^\\*mbsync\\*$")
                         (name . "^\\*straight-byte-compilation\\*$")
                         (name . "^\\*Shell Command Output\\*$")
                         (name . "^\\*XELB-DEBUG\\*$")))
            ("eglot" (name . "^\\*EGLOT"))))

        (add-hook 'ibuffer-hook
                  (lambda ()
                    (setq ibuffer-filter-groups (append drsl/ibuffer-filter-groups (ibuffer-project-generate-filter-groups))))))


;; Some sample EXWM config, no use for now. It adds Firefox.
;; (if *is-a-linux*
;;     (defvar drsl/ibuffer-filter-groups
;;       '(("shell" (or (mode . shell-mode)
;;                      (mode . vterm-mode)))
;;         ("mail" (or (mode . notmuch-show-mode)
;;                     (mode . notmuch-search-mode)
;;                     (mode . notmuch-hello-mode)
;;                     (name . "^\\*sent mail")
;;                     (name . "^\\*unsent mail")))
;;         ("firefox" (or (name . "^\\*firefox")
;;                        (name . "^\\*librewolf")))
;;         ("Helpful" (or (mode . helpful-mode)
;;                        (mode . help-mode)))
;;         ("emacs" (or (name . "^\\*scratch\\*$")
;;                      (name . "^\\*Messages\\*$")
;;                      (name . "^\\*Warnings\\*$")
;;                      (name . "^\\*Quail Completions\\*$")
;;                      (name . "^\\*Async-native-compile-log\\*$")
;;                      (name . "^\\*Backtrace\\*$")
;;                      (name . "^\\*Native-compile-Log\\*$")
;;                      (name . "^\\*info\\*$")
;;                      (name . "^\\*straight-process\\*$")
;;                      (name . "^\\*v2ray\\*$")
;;                      (name . "^\\*mbsync\\*$")
;;                      (name . "^\\*straight-byte-compilation\\*$")
;;                      (name . "^\\*Shell Command Output\\*$")
;;                      (name . "^\\*XELB-DEBUG\\*$")))
;;         ("eglot" (name . "^\\*EGLOT"))
;;         )))



;; For unicode characters that is not monospace, the alignment is a
;; bit off.
;; From https://www.emacswiki.org/emacs/IbufferMode#h5o-12.
(require 'ibuffer)
(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
