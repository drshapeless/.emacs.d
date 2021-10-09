;;; init-use-package.el ---
;;; Commentary:

;; use-package configurations.

;;; Code:

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(if *is-mac*
    (use-package exec-path-from-shell
      :ensure t
      :config
      (exec-path-from-shell-initialize)
      (if (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
          (progn
            (message "Native comp is available")
            (add-to-list 'exec-path (expand-file-name "/Applications/Emacs.app/Contents/MacOS/bin")
                         ;; If you use emacs-plus together with native-comp.
                         ;; (add-to-list 'exec-path (expand-file-name "/usr/local/Cellar/emacs-plus@28/28.0.50/bin")
                         )

            (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                           (when (getenv "LIBRARY_PATH")
                                             ":")
                                           (car (file-expand-wildcards
                                                 (expand-file-name "/usr/local/include/")))))
            ;; Only set after LIBRARY_PATH can find gcc libraries.
            (setq comp-deferred-compilation t))
        (message "Native comp is *not* available")))

  )

(provide 'init-use-package)
;;; init-use-package.el ends here
