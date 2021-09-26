;;; init-org-roam.el ---
;;; Commentary:

;;

;;; Code:

(setq org-roam-directory "~/org-roam")
(setq org-roam-v2-ack t)

;; This is old stuff.
;; (use-package org-roam
;;   ;; :hook
;;   ;; (after-init . org-roam-mode)
;;   :custom
;;   (org-roam-directory "~/org-roam/")
;;   :bind (:map org-roam-mode-map
;;               (("C-c r r" . org-roam)
;;                ("C-c r f" . org-roam-find-file))
;;               :map org-mode-map
;;               (("C-c r i" . org-roam-insert)
;;                ("C-c n I" . org-roam-insert-immediate))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
