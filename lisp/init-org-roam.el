;;; init-org-roam.el ---
;;; Commentary:

;; It is now org-roam 2.0.
;; Ignore this file, it has migrated into init-org.el.

;;; Code:

(use-package org-roam
  :bind (:map org-roam-mode-map
              (("C-c r f" . org-roam-node-find))))
(setq org-roam-directory "~/org-roam")
(setq org-roam-v2-ack t)
(org-roam-db-autosync-mode)



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
