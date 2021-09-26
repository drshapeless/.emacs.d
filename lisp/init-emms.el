;;; init-emms.el ---
;;; Commentary:

;;

;;; Code:

(use-package emms)
(require 'emms)

(emms-all)

(emms-default-players)
(setq emms-source-file-default-directory "/mnt/hdd/Music")
(emms-mode-line nil)
(emms-playing-time-enable-display)

(provide 'init-emms)
;;; init-emms.el ends here
