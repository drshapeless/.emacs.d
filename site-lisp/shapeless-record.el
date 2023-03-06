;;; shapeless-record.el --- Record music in Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; This is just a wrapper around ffmpeg and arecordmidi.

;;; Code:

(defun shapeless-record ()
  "Start recording."
  (interactive)
  (switch-to-buffer "*shapeless-record*")
  (shapeless-record-mode)
  (shapeless-record-display))

(define-derived-mode shapeless-record-mode special-mode "*shapeless-record*"
  (define-key shapeless-record-mode-map (kbd "s") #'shapeless-record-start-recording)
  (define-key shapeless-record-mode-map (kbd "N") #'shapeless-record-new-project)
  (define-key shapeless-record-mode-map (kbd "g") #'shapeless-record-display)
  (define-key shapeless-record-mode-map (kbd "SPC")
              #'shapeless-record-start-or-stop-recording)
  (define-key shapeless-record-mode-map (kbd "p") #'shapeless-record-previous-take)
  (define-key shapeless-record-mode-map (kbd "n") #'shapeless-record-next-take)
  (define-key shapeless-record-mode-map (kbd "v") #'shapeless-record-goto-directory)

  )

(defun shapeless-record-goto-directory ()
  (interactive)
  (find-file (concat shapeless-record-directory
                     shapeless-record-current-project-name)))

(defun shapeless-record-new-project (NAME)
  (interactive (list (read-string "record: ")))

  (shapeless-record-set-current-project-name NAME)
  (if (not (file-directory-p
            (concat shapeless-record-directory
                    shapeless-record-current-project-name)))
      (make-directory (concat shapeless-record-directory
                    shapeless-record-current-project-name))))

(defun shapeless-record-start-or-stop-recording ()
  "Start or stop recording.

It checks the `shapeless-record-status', if idle then start."
  (interactive)
  (if (string= "idle" shapeless-record-status)
      (shapeless-record-start-recording)
    (shapeless-record-stop-recording)))

(defun shapeless-record-previous-take ()
  "Previous take.

Decrease `shapeless-record-current-take' by 1.
Stop recording if is recording now."
  (interactive)
  (if (not (string= "idle" shapeless-record-status))
      (shapeless-record-stop-recording))
  (if (> shapeless-record-current-take 0)
      (setq shapeless-record-current-take
            (1- shapeless-record-current-take)))
  (shapeless-record-display))

(defun shapeless-record-next-take ()
  "Next take.

Increase `shapeless-record-current-take' by 1.
Stop recording if is recording now."
  (interactive)
  (if (not (string= "idle" shapeless-record-status))
      (shapeless-record-stop-recording))
  (setq shapeless-record-current-take
        (1+ shapeless-record-current-take))
  (shapeless-record-display))

(defun shapeless-record-start-recording ()
  "Start recording using ffmpeg and arecordmidi."
  (interactive)
  (setq shapeless-record-status "recording")
  (shapeless-record--start-recording-wav)
  (shapeless-record--start-recording-midi)
  (shapeless-record-display)
  (setq shapeless-record-start-time (current-time))
  (shapeless-record-start-auto-refresh))

(defun shapeless-record-stop-recording ()
  "Stop recording."
  (interactive)
  (setq shapeless-record-status "idle")
  (shapeless-record--stop-recording-wav)
  (shapeless-record--stop-recording-midi)
  (shapeless-record-display)
  (shapeless-record-stop-auto-refresh))

(defun shapeless-record--start-recording-wav ()
  "Start recording wav music with ffmpeg."
  (make-process
   :name "record-wav"
   :buffer nil
   :command (list "ffmpeg" "-y" "-f" "alsa" "-i" "sysdefault:CARD=Piano"
                  (concat shapeless-record-directory
                          shapeless-record-current-project-name
                          "/"
                          shapeless-record-current-project-name
                          "."
                          (format "%02d"
                                  shapeless-record-current-take)
                          ".wav"))))

(defun shapeless-record--stop-recording-wav ()
  "Stop recording wav music."
  (interrupt-process "record-wav"))

(defun shapeless-record--start-recording-midi ()
  "Start recording midi with arecordmidi."
  (make-process
   :name "record-midi"
   :buffer nil
   :command (list "arecordmidi" "-p" shapeless-record-midi-port
                  (concat shapeless-record-directory
                          shapeless-record-current-project-name
                          "/"
                          shapeless-record-current-project-name
                          "."
                          (format "%02d"
                           shapeless-record-current-take)
                          ".midi"))))

(defun shapeless-record--stop-recording-midi ()
  "Stop recording midi."
  (interrupt-process "record-midi"))

(defvar shapeless-record-directory
  (concat (getenv "HOME") "/recordings/")
  "Default directory for recordings.")

(defvar shapeless-record-current-project-name
  ""
  "The current project name.")

(defun shapeless-record-set-current-project-name (NAME)
  "Set the current project name into NAME."
  (setq shapeless-record-current-project-name (s-replace " " "_" NAME))
  (shapeless-record-display))

(defvar shapeless-record-current-take
  0
  "The current take of the file.")

(defvar shapeless-record-status
  "idle"
  "The current status of shapeless record.")

(defvar shapeless-record-midi-port
  "24:0"
  "The midi port for recording.

Check it with 'arecordmidi -l' in the cli.")

(defun shapeless-record-update-midi-port ()
  "Get the port for piano midi input."
  (interactive)
  (setq shapeless-record-midi-port
        (s-trim (shell-command-to-string
                 "arecordmidi -l | grep Piano | awk '{print $1;}'"))))

(defvar shapeless-record-timer
  nil
  "The timer object.")

(defvar shapeless-record-start-time
  nil
  "The start time of current recording.")

(defvar shapeless-record-end-time
  nil
  "The end time of the current recording.")

(defun shapeless-record-display ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Take\tStatus\t\tTime\tName\n")
    (insert (number-to-string shapeless-record-current-take))
    (insert "\t")
    (insert shapeless-record-status)
    (if (string= shapeless-record-status "idle")
        (insert "\t"))
    (insert "\t")
    (insert (format-time-string "%M:%S"
                                (time-subtract shapeless-record-end-time
                                               shapeless-record-start-time)))
    (insert "\t")
    (insert shapeless-record-current-project-name)
    (insert "\n")))

(defun shapeless-record-start-auto-refresh ()
  "Start auto refreshing."
  (setq shapeless-record-timer
        (run-at-time nil
                     1
                     (lambda ()
                       (if (string= (buffer-name (current-buffer))
                                    "*shapeless-record*")
                           (progn
                             (setq shapeless-record-end-time (current-time))
                             (shapeless-record-display)))))))

(defun shapeless-record-stop-auto-refresh ()
  "Stop auto refreshing."
  (cancel-timer shapeless-record-timer))

(provide 'shapeless-record)
;;; shapeless-record.el ends here
