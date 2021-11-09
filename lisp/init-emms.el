;;; init-emms.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(leaf emms
  :require t
  :config
  (emms-all)
  (setq emms-source-file-default-directory "/mnt/hdd/Music/")
  (emms-mode-line nil)
  (emms-playing-time-enable-display))

(provide 'init-emms)
;;; init-emms.el ends here
