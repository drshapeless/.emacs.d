;;; init-emms.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Listen music in Emacs. Make sure you have mpv installed.

;;; Code:

(straight-use-package 'emms)
(require 'emms)
(emms-default-players)
(emms-all)
;; mpv for whatever reason fucked up.
(setq emms-player-list '(emms-player-vlc)
      emms-info-functions '(emms-info-native))
(setq emms-source-file-default-directory "~/music/")
(emms-mode-line nil)
(emms-playing-time-enable-display)

(keymap-global-set "C-c m m" #'emms)
(keymap-global-set "C-c m <SPC>" #'emms-pause)
(keymap-global-set "C-c m p" #'emms-previous)
(keymap-global-set "C-c m n" #'emms-next)
(keymap-global-set "C-c m s" #'emms-shuffle)
;; Play from the beginning of current song.
(keymap-global-set "C-c m b"
                   (lambda () (interactive) (emms-stop) (emms-start)))
(keymap-global-set "C-c m c" #'emms-show)
(keymap-global-set "C-c m d" #'emms-add-directory)
(keymap-global-set "C-c m f" #'emms-play-dired)

(provide 'init-emms)
;;; init-emms.el ends here
