;;; init-shell.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Disable shell from echoing.
(add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))

;; Use zsh as default shell in Emacs.
(setq explicit-shell-file-name "/bin/zsh")
(setenv "SHELL" "/bin/zsh")

;; From emacs wiki, https://www.emacswiki.org/emacs/ExecPath
;; This is particularly useful if you edited the path variable, e.g. in your .zshrc.
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                                    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if *is-a-mac*
    (set-exec-path-from-shell-PATH))

(provide 'init-shell)
;;; init-shell.el ends here
