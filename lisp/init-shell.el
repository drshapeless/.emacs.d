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

(elpaca exec-path-from-shell
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; From emacs wiki, https://www.emacswiki.org/emacs/ExecPath
;; This is particularly useful if you edited the path variable, e.g. in your .zshrc.
;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' `exec-path' and PATH environment variable to match
;; that used by the user's shell.

;; This is particularly useful under Mac OS X and macOS, where GUI
;; apps are not started from a shell."
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$" "" (shell-command-to-string
;;                                           "$SHELL --login -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; No reason to make things complicated, add what I need to path.
;; (add-to-list 'exec-path (concat (getenv "HOME") "/go/bin"))
;; (add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
;; (add-to-list 'exec-path (concat (getenv "HOME") "/.venv/bin"))
;; (add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
;; (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
;; (add-to-list 'exec-path (concat (getenv "HOME") "/src/emsdk"))
;; (add-to-list 'exec-path (concat (getenv "HOME") "/src/emsdk/upstream/emscripten"))

;; Shell switcher
(elpaca
    shell-switcher
  (require 'shell-switcher)
  (setq shell-switcher-mode t)
  (setq shell-switcher-new-shell-function #'shell-switcher-make-eshell))

;; vterm
(elpaca
    vterm
  (require 'vterm)
  (setq vterm-buffer-name-string "vterm %s")
  (setq vterm-always-compile-module t)
  (keymap-set vterm-mode-map "C-q" #'vterm-send-next-key)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path)))))

;; multi-vterm
(elpaca
    multi-vterm
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
  (keymap-set vterm-mode-map "s-n" #'multi-vterm-next)
  (keymap-set vterm-mode-map "s-p" #'multi-vterm-prev))

;; (elpaca
;;     vterm-toggle
;;   (require 'vterm-toggle)
;;   (setq vterm-toggle-fullscreen-p nil)
;;   (add-to-list 'display-buffer-alist
;;                '((lambda (buffer-or-name _)
;;                    (let ((buffer (get-buffer buffer-or-name)))
;;                      (with-current-buffer buffer
;;                        (or (equal major-mode 'vterm-mode)
;;                            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;;                  (display-buffer-reuse-window display-buffer-same-window))))


;; Eat.
(elpaca
    eat
  (require 'eat)
  (keymap-global-set "C-c s" #'eat)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  )

(provide 'init-shell)
;;; init-shell.el ends here
