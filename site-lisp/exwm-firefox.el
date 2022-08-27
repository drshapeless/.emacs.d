;;; exwm-firefox.el --- Firefox + EXWM     -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Version: 0.3
;; URL: https://github.com/ieure/exwm-firefox
;; Package-Requires: ((emacs "25") (s "1.12.0") (exwm "0.22.1") (exwm-firefox-core "20190608.2213"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds enhanced support for Firefox under EXWM.
;; Keybindings intentionally mirror other Emacs navigation controls.

;; To enable it, run M-x exwm-firefox-mode RET

;; - Navigate forwards (=C-c C-f=) and backwards (=C-c C-b=) in
;;   browsing history.
;; - Open a new window in an Emacs split (=C-c C-n=).
;; - Open a new private window in an Emacs split (=C-c C-p=).
;; - Detach the current tab into an Emacs split window (=C-c C-d=).
;;   Requires tabdetach extension.
;; - Merge the detached tab back into its parent window (=C-c C-=).
;;   Requires tabdetach extension.

;;; Code:

(require 'exwm-firefox-core)
(require 's)
(require 'ert)

(defvar exwm-firefox--title-re ""
  "Regular expression to match a Firefox page title.

This is set when `exwm-firefox-class->name-alist' is customized.")

(defgroup exwm-firefox nil
  "Customizing EXWM enhancements for Firefox-like browsers."
  :prefix "exwm-firefox-"
  :group 'exwm)

(defcustom exwm-firefox-class->name-alist
  '(("Mozilla Firefox" . "firefox")
    ("Firefox-esr" . "firefox")
    ("LibreWolf" . "librewolf")
    ("librewolf-default" . "librewolf"))
  "Alist for mapping client classes to simplified browser names.

The car is the X11 client class, which is the value of
`exwm-class-name' for the browser's buffer.  The cdr is the simplified
name to use when building the buffer name, for example '*firefox: Page
title*'"

  :group 'exwm-firefox
  :type '(alist :key-type (string :tag "EXWM client class name")
                :value-type (string :tag "Name for buffers"))
  :set (lambda (sym val)
         (set sym val)
         (setq exwm-firefox--title-re (eval
                                       `(rx bol
                                            ;; Page title.  Optional because it's not set on blank pages.
                                            (optional (group (* anything)) (seq (+ space) (or "-" "—") (+ space)))
                                            ;; Always present.
                                            (seq (or ,@(mapcar #'car val)))
                                            ;; Present in private windows
                                            (optional (group " (Private Browsing)"))
                                            eol)))))

(defvar exwm-firefox--intercept nil
  "The function to call when a new Firefox window is created.")

(defvar exwm-firefox-keymap
  (let ((map (copy-keymap exwm-mode-map)))
    (define-key map "\C-c\C-f" 'exwm-firefox-core-history-forward)
    (define-key map "\C-c\C-b" 'exwm-firefox-core-history-back)
    (define-key map "\C-c\C-n" 'exwm-firefox-split-window)
    (define-key map "\C-c\C-p" 'exwm-firefox-split-private)
    (define-key map "\C-c\C-d" 'exwm-firefox-split-detach)
    (define-key map "\C-c\C-g" 'exwm-firefox-merge)
    map)
  "Keymap applied in Firefox windows.")

(defun exwm-firefox? ()
  "Does the current buffer contain a Firefox window?"
  (not (null (assoc exwm-class-name exwm-firefox-class->name-alist))))

(defun exwm-firefox--setup-keymap-hook ()
  "Configure Firefox keymap for EXWM."
  (when (exwm-firefox?)
    (use-local-map exwm-firefox-keymap)))

(defun exwm-firefox--title->buffer-name (title)
  (concat "*"
          (save-match-data
            (if (string-match exwm-firefox--title-re title)
                (concat
                 (or (cdr (assoc exwm-class-name exwm-firefox-class->name-alist)) "browser")
                 (if (match-string 2 title) "-private" "")
                 (if-let ((page-title (match-string 1 title)))
                     (concat ": " page-title)
                   ""))
              "firefox"))
          "*"))

(defun exwm-firefox--update-title ()
  (when (exwm-firefox?)
    (let ((name (exwm-firefox--title->buffer-name exwm-title)))
      (unless (s-starts-with? name (buffer-name))
        (rename-buffer (generate-new-buffer-name name))))))

(defun exwm-firefox--split (old-window-config)
  "Move a new Firefox window into a split.

   OLD-WINDOW-CONFIG is the window confguration at the time the
   split window was created.

   Returns nil."
  (let ((new-firefox (current-buffer)))
    (set-window-configuration old-window-config)
    (switch-to-buffer-other-window new-firefox)
    (exwm-firefox-core-focus-search-bar)
    (setq exwm-firefox--intercept nil)))

(defun exwm-firefox--intercept-hook ()
  "Run an action the next time a Firefox window is created."
  (if-let ((callback (and (exwm-firefox?) exwm-firefox--intercept)))
      (funcall callback)))

(defun exwm-firefox--intercept-next (data callback)
  "Perform an action the next time a Firefox window is created.

   Calls function CALLBACK with DATA as its only argument."

  (unless (exwm-firefox?)
    (error "Not a Firefox window"))
  (when exwm-firefox--intercept
    (warn "Already intercepting"))

  (setq exwm-firefox--intercept (apply-partially callback data))

  ;; If nothing happens in 3 seconds, reset the state
  (thread-last (lambda () (setq exwm-firefox--intercept nil))
    (run-with-timer 3 nil)))

(defun exwm-firefox-split-window ()
  "Create a new Firefox window in a split."
  (interactive)
  (exwm-firefox--intercept-next (current-window-configuration)
                                'exwm-firefox--split)
  (exwm-firefox-core-window-new))

(defun exwm-firefox-split-private ()
  "Create a new Firefox private window in a split.

   With ARG prefix, display the window in that workspace."
  (interactive)
  (exwm-firefox--intercept-next (current-window-configuration)
                                'exwm-firefox--split)
  (exwm-firefox-core-window-new-private))

(defun exwm-firefox-split-detach ()
  "Detach the current tab into a new split window.

   With no ARG, create the new window in a split in the current workspace.

   This requires the tabdetach extension to work."
  (interactive)
  (exwm-firefox--intercept-next (current-window-configuration)
                                'exwm-firefox--split)
  (exwm-input--fake-key ?\M-\S-d))

(defun exwm-firefox-merge ()
  "Merge the current tab into its parent window.

   This requires the tabdetach extension to work."
  (interactive)
  (exwm-input--fake-key ?\M-\S-m))

(define-minor-mode exwm-firefox-mode
  "Minor mode to enhance Firefox in EXWM."
  :init-value nil
  :lighter nil
  :keymap nil
  :keymap nil
  :group 'tools
  :global t
  (setq exwm-firefox--intercept nil)
  (if exwm-firefox-mode
      (progn
        (add-hook 'exwm-manage-finish-hook 'exwm-firefox--setup-keymap-hook)
        (add-hook 'exwm-manage-finish-hook 'exwm-firefox--intercept-hook)
        (add-hook 'exwm-update-title-hook 'exwm-firefox--update-title))
    (remove-hook 'exwm-manage-finish-hook 'exwm-firefox--setup-keymap-hook)
    (remove-hook 'exwm-manage-finish-hook 'exwm-firefox--intercept-hook)
    (remove-hook 'exwm-update-title-hook 'exwm-firefox--update-title)))

(ert-deftest exwm-firefox--test--exwm-firefox? ()
  (should (exwm-firefox? "Mozilla Firefox"))
  (should (exwm-firefox? "LibreWolf")))

(ert-deftest exwm-firefox--test--title->buffer-name ()
  (should (string= "*firefox*" (exwm-firefox--title->buffer-name "Mozilla Firefox")))
  (should (string= "*firefox-private*" (exwm-firefox--title->buffer-name "Mozilla Firefox (Private Browsing)")))
  (should (string= "*firefox: DuckDuckGo — Privacy, simplified.*"
                   (exwm-firefox--title->buffer-name "DuckDuckGo — Privacy, simplified. - Mozilla Firefox")))
  (should (string= "*firefox: DuckDuckGo — Privacy, simplified.*"
                   (exwm-firefox--title->buffer-name "DuckDuckGo — Privacy, simplified. — Mozilla Firefox")))
  (should (string= "*firefox: ieure/scratch-el: Scratch buffers for Emacs*"
             (exwm-firefox--title->buffer-name "ieure/scratch-el: Scratch buffers for Emacs - Mozilla Firefox"))))

(provide 'exwm-firefox)
;;; exwm-firefox.el ends here
