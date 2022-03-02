;;; init-pixel-scroll-precision.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; From Emacs 29, it comes with a built in `pixel-scroll-precision-mode'.
;; But the direction of the scroll is not customizable.

;; It makes life difficult for those who are used to mac-like
;; scrolling experience.

;;; Code:

(pixel-scroll-precision-mode 1)

(defvar pixel-scroll-precision-direction
  'natural
  "Supports two directions, natural or normal.

'natural is mac-like scrolling.
'normal is windows-like scrolling.")

(defun pixel-scroll-precision (event)
  "Scroll the display vertically by pixels according to EVENT.
Move the display up or down by the pixel deltas in EVENT to
scroll the display according to the user's turning the mouse
wheel."
  (interactive "e")
  (let ((window (mwheel-event-window event))
        (current-window (selected-window)))
    (when (framep window)
      (setq window (frame-selected-window window)))
    (if (and (nth 4 event))
        (let ((delta (round (cdr (nth 4 event)))))
          (unless (zerop delta)
            (if (> (abs delta) (window-text-height window t))
                (mwheel-scroll event nil)
              (with-selected-window window
                (if (and pixel-scroll-precision-large-scroll-height
                         (> (abs delta)
                            pixel-scroll-precision-large-scroll-height)
                         (let* ((kin-state (pixel-scroll-kinetic-state))
                                (ring (aref kin-state 0))
                                (time (aref kin-state 1)))
                           (or (null time)
                               (> (- (float-time) time) 1.0)
                               (and (consp ring)
                                    (ring-empty-p ring)))))
                    (progn
                      (let ((kin-state (pixel-scroll-kinetic-state)))
                        (aset kin-state 0 (make-ring 30))
                        (aset kin-state 1 nil))
                      (pixel-scroll-precision-interpolate delta current-window))
                  (condition-case nil
                      (progn
                        (cond ((eq pixel-scroll-precision-direction 'natural)
                               (if (< delta 0)
                                   (pixel-scroll-precision-scroll-up (- delta))
                                 (pixel-scroll-precision-scroll-down delta)))
                              ((eq drsl/pixel-scroll-precision-direction 'normal)
                               (if (< delta 0)
                                   (pixel-scroll-precision-scroll-down (- delta))
                                 (pixel-scroll-precision-scroll-up delta))))

                        (pixel-scroll-accumulate-velocity delta))
                    ;; Do not ding at buffer limits.  Show a message instead.
                    (beginning-of-buffer
                     (message (error-message-string '(beginning-of-buffer))))
                    (end-of-buffer
                     (message (error-message-string '(end-of-buffer))))))))))
      (mwheel-scroll event nil))))

(provide 'init-pixel-scroll-precision)
;;; init-pixel-scroll-precision.el ends here
