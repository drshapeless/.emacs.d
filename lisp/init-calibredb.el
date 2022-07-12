;;; init-calibredb.el --- Calibre -*- lexical-binding: t -*-

;;; Commentary:

;; Manage ebooks.

;;; Code:

(straight-use-package 'calibredb)
(require 'calibredb)
(setq calibredb-root-dir "~/calibre")
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
(setq calibredb-add-delete-original-file "yes")
(keymap-set calibredb-search-mode-map "n" #'calibredb-show-next-entry)
(keymap-set calibredb-search-mode-map "p" #'calibredb-show-previous-entry)
(keymap-set calibredb-search-mode-map "M-n" #'calibredb-virtual-library-next)
(keymap-set calibredb-search-mode-map "M-p" #'calibredb-virtual-library-previous)

(provide 'init-calibredb)
;;; init-calibredb.el ends here
