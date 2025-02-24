;;; init-swift.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Swift mode is really bad in Emacs.

;;; Code:

(elpaca
    swift-ts-mode
  (require 'swift-ts-mode)
  (keymap-set swift-ts-mode-map "C-c p b" #'xcode-build)
  (keymap-set swift-ts-mode-map "C-c p r" #'xcode-run)
  (keymap-set swift-ts-mode-map "C-c p t" #'xcode-test)
  (keymap-set swift-ts-mode-map "C-c p o" #'xcode-open-current-file))

(defun print-swift-var-under-point()
  (interactive)
  (if (string-match-p (string (preceding-char)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
      (backward-sexp)
    nil)
  (kill-sexp)
  (yank)
  (move-end-of-line nil)
  (newline)
  (insert "print(\"")
  (yank)
  (insert ": \\(")
  (yank)
  (insert ")\")")
  (indent-for-tab-command))

(defun xcode-build()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
(defun xcode-run()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
(defun xcode-test()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))


(defun xcode-open-current-file()
  (interactive)
  (shell-command-to-string
   (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name))))


(provide 'init-swift)
;;; init-swift.el ends here
