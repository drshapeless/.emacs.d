;;; init-shell.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Packages about shell.

;; shell-switcher, vterm, multi-vterm.

;;; Code:

;; Disable shell from echoing.
(require 'comint)
(setq comint-process-echoes t)

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
                                          "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; No reason to make things complicated, add what I need to path.
(add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
(add-to-list 'exec-path (concat (getenv "HOME") "/bin"))

;; Shell switcher
(straight-use-package 'shell-switcher)
(require 'shell-switcher)
(setq shell-switcher-mode t)
(setq shell-switcher-new-shell-function #'shell-switcher-make-shell)

;; Coterm
(straight-use-package 'coterm)
;; Coterm enable TUI like program to be run in shell.
(coterm-mode)

;; vterm
(straight-use-package 'vterm)
(require 'vterm)
(setq vterm-buffer-name-string "vterm %s")
(setq vterm-always-compile-module t)
(keymap-set vterm-mode-map "C-q" #'vterm-send-next-key)
(add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

;; multi-vterm
(straight-use-package 'multi-vterm)
(require 'multi-vterm)
(defun drsl/new-vterm-or-existing-vterm ()
  "Create new or open existing vterm buffer.

Create a new vterm buffer if the current buffer is `vterm-mode'.
Open an existing vterm buffer if the current buffer is not `vterm-mode'."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (multi-vterm)
    (multi-vterm-next)))

(keymap-global-set "C-c v" #'drsl/new-vterm-or-existing-vterm)
(keymap-set vterm-mode-map "H-n" #'multi-vterm-next)
(keymap-set vterm-mode-map "H-p" #'multi-vterm-prev)

(straight-use-package 'vterm-toggle)
(require 'vterm-toggle)
;; (keymap-global-set "C-c s" 'vterm-toggle)
;; (keymap-set vterm-mode-map "H-n" 'vterm-toggle-forward)
;; (keymap-set vterm-mode-map "H-p" 'vterm-toggle-backward)
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                 (let ((buffer (get-buffer buffer-or-name)))
                   (with-current-buffer buffer
                     (or (equal major-mode 'vterm-mode)
                         (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
               (display-buffer-reuse-window display-buffer-same-window)))

;; vterm-capf
;; (straight-use-package '(vterm-capf :type git :host github :repo "twlz0ne/vterm-capf"))
;; (require 'vterm-capf)
;; (setq vterm-capf-frontend 'corfu)
;; (add-hook 'vterm-mode-hook #'vterm-capf-mode)

;; This is about eterm.
(straight-use-package 'eterm-256color)
(require 'eterm-256color)
(add-hook 'term-mode-hook #'eterm-256color-mode)

;; Eat.
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(require 'eat)
;; (eat-compile-terminfo)
(setq eat-term-name "xterm-256color")
(keymap-global-set "C-c s" #'eat)

(provide 'init-shell)
;;; init-shell.el ends here
