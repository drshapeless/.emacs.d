emacs-lisp-mode
(e ";;; " (file-name-nondirectory buffer-file-name)
   " --- "  " -*- lexical-binding: t -*-" n n
   ";;; Commentary:" n n
   ";; "  n n
   ";;; Code:" n n n n
   "(provide '" (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ")" n
   ";;; " (file-name-nondirectory buffer-file-name) " ends here")
