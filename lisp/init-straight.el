;;; init-straight.el --- An alternative Emacs package manager. -*- lexical-binding: t -*-

;;; Commentary:

;; straight.el replace the built-in package.el

;; It respects user configuration to clone the packages from
;; source. Great for package developers.

;;; Code:

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight-x)

(straight-override-recipe '(compat :type git :host github :repo "emacs-compat/compat"))

(provide 'init-straight)
;;; init-straight.el ends here
