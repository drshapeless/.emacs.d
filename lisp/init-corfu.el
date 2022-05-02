;;; init-corfu.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Another completion framework other than company mode.
;; Corfu reuses a lot of internal Emacs ways of doing things.

;;; Code:

(leaf corfu
  :require t
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator
        corfu-auto-prefix 1
        corfu-auto-delay 0)
  (global-corfu-mode))

;; Corfu for eglot.
(setq completion-category-overrides '((eglot (styles orderless))))

(load "extensions/corfu-history")
(corfu-history-mode)
(add-to-list 'savehist-additional-variables 'corfu-history)

;; These will lead to a very weird bug.

;; For a dual monitor setup, the completion frame will only display on
;; the leaf monitor.

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



(provide 'init-corfu)
;;; init-corfu.el ends here
