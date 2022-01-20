;;; init-framemove.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; I have no idea how framemove determine which monitor is at which
;; position. By observation, the monitor turn on first after startx is
;; the left one, which makes absolutely no sense.

;; An ugly solution is to swap the left and right by replacing the
;; function.

;;; Code:

;; framemove is from emacs wiki, not a package.
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; Reboot solves the following bug. But after you restart Emacs, you
;; still need this.

;; For some stupid unknown reasons, framemove think my right monitor
;; is on the left. I just swap the shit.
(defadvice windmove-do-window-select (around framemove-do-window-select-wrapper activate)
  "Let windmove do its own thing, if there is an error, try
other frame."
  (condition-case err
      ad-do-it
    (error
     (cond ((eq 'left (ad-get-arg 0)) (fm-next-frame 'right))
           ((eq 'right (ad-get-arg 0)) (fm-next-frame 'left))
           (t (error (error-message-string err)))))))

;; This is the original, only useful when in first boot.
(defun drsl/framemove-use-original ()
  (interactive)
  (defadvice windmove-do-window-select (around framemove-do-window-select-wrapper activate)
    "Let windmove do its own thing, if there is an error, try framemove in that direction."
    (condition-case err
        ad-do-it
      (error
       (if framemove-hook-into-windmove
           (fm-next-frame (ad-get-arg 0))
         (error (error-message-string err)))))))

(provide 'init-framemove)
;;; init-framemove.el ends here
