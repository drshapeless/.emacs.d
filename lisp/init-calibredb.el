;;; init-calibredb.el --- Calibre -*- lexical-binding: t -*-

;;; Commentary:

;; Manage ebooks.

;;; Code:

(leaf calibredb
  :require t
  :config
  (setq calibredb-root-dir "~/calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-add-delete-original-file "yes")
  :bind
  (:calibredb-search-mode-map
   ("n" . calibredb-show-next-entry)
   ("p" . calibredb-show-previous-entry)
   ("M-n" . calibredb-virtual-library-next)
   ("M-p" . calibredb-virtual-library-previous)))

(provide 'init-calibredb)
;;; init-calibredb.el ends here
