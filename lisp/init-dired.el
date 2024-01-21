;;; init-dired.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Use `gls' on macos to enable `--group-directories-first' tag.

;; Some custom functions to enable sorting in dired.

;;; Code:

;; This probably should not be here, but whatever.
;; Never ask about opening big file, I have a lot of ram.
(setq large-file-warning-threshold nil)

(require 'dired)

;; The following does not work out of the box for a mac, since the ls
;; ship with mac does not have to option to sort directories first.

;; The work around for this is to install coreutils via homebrew.
(if *is-a-mac*
    (setq insert-directory-program "gls" dired-use-ls-dired t))

(defun drsl/dired-listing-switches-toggle ()
  "Toggle between --group-directories-first."
  (interactive)
  (if (string= "-ahl" dired-listing-switches)
      (setq dired-listing-switches "--group-directories-first -ahl")
    (setq dired-listing-switches "-ahl"))
  (message (format "dired-listing-switches: %s" dired-listing-switches)))

(keymap-set dired-mode-map "b" #'drsl/dired-listing-switches-toggle)

;; "--group-directories-first" must be at front, otherwise concat will
;; break.
(setq dired-listing-switches "-ahl")
;; If tramp into macOS shows a blank dired buffer, mostly is because of the ls in macOS does not support the switch --group-directories-first.
(setq dired-mode-hook 'drsl/dired-sort-set-mode-line)

;; A custom sorting mechanism.
(defun drsl/dired-sort-size ()
  "Dired sort by size."
  (setq dired-actual-switches (concat dired-listing-switches "S")))

(defun drsl/dired-sort-extension ()
  "Dired sort by extension."
  (setq dired-actual-switches (concat dired-listing-switches "X")))

(defun drsl/dired-sort-ctime ()
  "Dired sort by create time."
  (setq dired-actual-switches (concat dired-listing-switches "ct")))

(defun drsl/dired-sort-utime ()
  "Dired sort by access time."
  (setq dired-actual-switches (concat dired-listing-switches "ut")))

(defun drsl/dired-sort-name ()
  "Dired sort by name."
  (setq dired-actual-switches (concat dired-listing-switches "")))

(defvar drsl/dired-sort-mode 1
  "The current dired sorting mode.
1 is by name.
2 is by create time.
3 is by access time.
4 is by size.
5 is by extension.
Setting it to other values may have undetermined effect.")

;; Re-define the dired-sort-toggle function. Do not prefix it.
(defun dired-sort-toggle ()
  "Toggle between sort by name, create time, access time, size, extension."
  ;; Loop the mode list.
  (if (< drsl/dired-sort-mode 5)
      (setq drsl/dired-sort-mode (1+ drsl/dired-sort-mode))
    (setq drsl/dired-sort-mode 1))
  ;; Change the ls flags.
  (cond ((= drsl/dired-sort-mode 1)
         (drsl/dired-sort-name))
        ((= drsl/dired-sort-mode 2)
         (drsl/dired-sort-ctime))
        ((= drsl/dired-sort-mode 3)
         (drsl/dired-sort-utime))
        ((= drsl/dired-sort-mode 4)
         (drsl/dired-sort-size))
        ((= drsl/dired-sort-mode 5)
         (drsl/dired-sort-extension)))
  ;; Update mode line.
  (drsl/dired-sort-set-mode-line)
  (revert-buffer))

;; A custom modeline update function.
(defun drsl/dired-sort-set-mode-line ()
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (concat "Dired by "
                  (cond ((= drsl/dired-sort-mode 1)
                         "name")
                        ((= drsl/dired-sort-mode 2)
                         "create")
                        ((= drsl/dired-sort-mode 3)
                         "access")
                        ((= drsl/dired-sort-mode 4)
                         "size")
                        ((= drsl/dired-sort-mode 5)
                         "extension"))))
    (force-mode-line-update)))

(require 'dired-x)
(setq dired-guess-shell-alist-user '(("\\.mkv\\'"  "mpv")
                                     ("\\.avi\\'"  "mpv")
                                     ("\\.mp4\\'"  "mpv")
                                     ("\\.m4v\\'"  "mpv")
                                     ("\\.flv\\'"  "mpv")
                                     ("\\.wmv\\'"  "mpv")
                                     ("\\.mpg\\'"  "mpv")
                                     ("\\.mpeg\\'" "mpv")
                                     ("\\.webm\\'" "mpv")
                                     ("\\.flac\\'" "mpv")
                                     ("\\.wav\\'"  "mpv")
                                     ("\\.xlsx\\'" "soffice")))

;; Omit .. and . directories.
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;; Omit junk files in macos.
(setq dired-omit-files
      (concat dired-omit-files "\\|^.DS_Store$\\|^[.]_"))

(provide 'init-dired)
;;; init-dired.el ends here
