;;; init-mentor.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf mentor
  :config
  (setq mentor-rtorrent-download-directory "/mnt/hdd/Downloads/")
  ;; (setq mentor-rtorrent-external-rpc "/home/jacky/.rtorrent-rpc.socket")
  (setq mentor-rtorrent-keep-session t)
  )

(provide 'init-mentor)
;;; init-mentor.el ends here
