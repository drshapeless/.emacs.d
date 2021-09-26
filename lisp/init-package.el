;;; init-package.el ---
;;; Commentary:

;; Load melpa and elpa.

;;; Code:

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; Orgmode elpa is deprecated since org version 9.5.
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(provide 'init-package)
;;; init-package.el ends here
