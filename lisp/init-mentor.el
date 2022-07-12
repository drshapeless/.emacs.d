;;; init-mentor.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Torrenting in Emacs, depends on rTorrents.

;;; Code:

(straight-use-package 'mentor)
(require 'mentor)
(setq mentor-rtorrent-download-directory "/mnt/hdd/Downloads/")
(setq mentor-rtorrent-keep-session t)
(keymap-global-set "C-c t" #'mentor)

(provide 'init-mentor)
;;; init-mentor.el ends here
