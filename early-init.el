;;; early-init.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(setenv "LIBRARY_PATH"
        (mapconcat 'identity
                   '(
                     "/opt/homebrew/opt/gcc/lib/gcc/16"
                     "/opt/homebrew/opt/libgccjit/lib/gcc/16"
                     "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin25/16/")
                   ":"))

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
