;;; init-embark.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; I do not have a clear idea of what embark is doing.

;;; Code:

(elpaca
 embark
 (require 'embark)
 (keymap-global-set "C-." #'embark-act)
 (keymap-global-set "C-;" #'embark-dwim)
 (keymap-global-set "C-h B" #'embark-bindings)
 ;; Optionally replace the key help with a completing-read interface
 (setq prefix-help-command #'embark-prefix-help-command)
 ;; Hide the mode line of the Embark live/completions buffers
 (add-to-list 'display-buffer-alist
              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                nil
                (window-parameters (mode-line-format . none)))))

(elpaca
 embark-consult
 (require 'embark-consult)
 (add-hook 'embark-collect-mode #'consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here
