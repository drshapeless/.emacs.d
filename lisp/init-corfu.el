;;; init-corfu.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Another completion framework other than company mode.
;; Corfu reuses a lot of internal Emacs ways of doing things.

;;; Code:

(straight-use-package 'corfu)
(require 'corfu)
(setq corfu-auto t
      corfu-quit-no-match 'separator
      corfu-auto-prefix 1
      corfu-auto-delay 0
      corfu-min-width 80
      corfu-max-width corfu-min-width
      corfu-count 14
      corfu-scroll-margin 4
      corfu-cycle nil
      corfu-preselect-first t
      corfu-echo-documentation t)
(global-corfu-mode)

;; Corfu for eglot.
(setq completion-category-overrides '((eglot (styles orderless))))

(corfu-history-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)

;; These will lead to a very weird bug.

;; For a dual monitor setup, the completion frame will only display on
;; the left monitor.

;; Corfu for EXWM.
;; (advice-add #'corfu--make-frame :around
;;             (defun +corfu--make-frame-a (oldfun &rest args)
;;               (cl-letf (((symbol-function #'frame-parent)
;;                          (lambda (frame)
;;                            (or (frame-parameter frame 'parent-frame)
;;                                exwm-workspace--current))))
;;                 (apply oldfun args))
;;               (when exwm--connection
;;                 (set-frame-parameter corfu--frame 'parent-frame nil))))

;; (advice-add #'corfu--popup-redirect-focus :override
;;             (defun +corfu--popup-redirect-focus-a ()
;;               (redirect-frame-focus corfu--frame
;;                                     (or (frame-parent corfu--frame)
;;                                         exwm-workspace--current))))

(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(unless (display-graphic-p)
  (corfu-terminal-mode 1))

(provide 'init-corfu)
;;; init-corfu.el ends here
