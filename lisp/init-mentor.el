;;; init-mentor.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Torrenting in Emacs, depends on rTorrents.

;;; Code:

(elpaca
 mentor
 (require 'mentor)
 (setq mentor-rtorrent-download-directory "~/torrent-downloads/")
 (setq mentor-rtorrent-keep-session t)
 (keymap-global-set "C-c t" #'mentor))

(provide 'init-mentor)
;;; init-mentor.el ends here
