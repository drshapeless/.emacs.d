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

;; For the bug in commit 1c53066
(defun corfu--make-frame (frame x y width height buffer)
  "Show BUFFER in child frame at X/Y with WIDTH/HEIGHT.
FRAME is the existing frame."
  (when-let (((frame-live-p frame))
             (timer (frame-parameter frame 'corfu--hide-timer)))
    (cancel-timer timer)
    (set-frame-parameter frame 'corfu--hide-timer nil))
  (let* ((window-min-height 1)
         (window-min-width 1)
         (inhibit-redisplay t)
         (x-gtk-resize-child-frames corfu--gtk-resize-child-frames)
         (before-make-frame-hook)
         (after-make-frame-functions)
         (parent (window-frame)))
    (unless (and (frame-live-p frame)
                 (eq (frame-parent frame)
                     (and (not (bound-and-true-p exwm--connection)) parent))
                 ;; If there is more than one window, `frame-root-window' may
                 ;; return nil.  Recreate the frame in this case.
                 (window-live-p (frame-root-window frame)))
      (when frame (delete-frame frame))
      (setq frame (make-frame
                   `((parent-frame . ,parent)
                     (minibuffer . ,(minibuffer-window parent))
                     (width . 0) (height . 0) (visibility . nil)
                     ,@corfu--frame-parameters))))
    ;; XXX HACK Setting the same frame-parameter/face-background is not a nop.
    ;; Check before applying the setting. Without the check, the frame flickers
    ;; on Mac. We have to apply the face background before adjusting the frame
    ;; parameter, otherwise the border is not updated.
    (let ((new (face-attribute 'corfu-border :background nil 'default)))
      (unless (equal (face-attribute 'internal-border :background frame 'default) new)
        (set-face-background 'internal-border new frame)))
    ;; Reset frame parameters if they changed.  For example `tool-bar-mode'
    ;; overrides the parameter `tool-bar-lines' for every frame, including child
    ;; frames.  The child frame API is a pleasure to work with.  It is full of
    ;; lovely surprises.
    (when-let ((params (frame-parameters frame))
               (reset (seq-remove
                       (lambda (p) (equal (alist-get (car p) params) (cdr p)))
                       `((background-color
                          . ,(face-attribute 'corfu-default :background nil 'default))
                         (font . ,(frame-parameter parent 'font))
                         ,@corfu--frame-parameters))))
      (modify-frame-parameters frame reset))
    (let ((win (frame-root-window frame)))
      (unless (eq (window-buffer win) buffer)
        (set-window-buffer win buffer))
      ;; Disallow selection of root window (gh:minad/corfu#63)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-parameter win 'no-other-window t)
      ;; Mark window as dedicated to prevent frame reuse (gh:minad/corfu#60)
      (set-window-dedicated-p win t))
    (redirect-frame-focus frame parent)
    (set-frame-size frame width height t)

    (if (bound-and-true-p exwm--connection)
        (let ((geo (frame-monitor-geometry exwm-workspace--current)))
          (set-frame-position
           frame
           (+ x (car geo))
           (+ y (car (cdr geo)))))
      (unless (equal (frame-position frame) (cons x y))
        (set-frame-position frame x y))))
  (make-frame-visible frame)
  ;; Unparent child frame if EXWM is used, otherwise EXWM buffers are drawn on
  ;; top of the Corfu child frame.
  (when (and (bound-and-true-p exwm--connection) (frame-parent frame))
    (set-frame-parameter frame 'parent-frame nil))
  frame)

(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(unless (display-graphic-p)
  (corfu-terminal-mode 1))

(provide 'init-corfu)
;;; init-corfu.el ends here
