;;; init-ebuild.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Major mode for editing Gentoo ebuild

;;; Code:

(elpaca
    (ebuild-mode :host github :repo "emacsmirror/ebuild-mode")
  (require 'ebuild-mode))

(provide 'init-ebuild)
;;; init-ebuild.el ends here
