emacs-lisp-mode
(e ";;; " (file-name-nondirectory buffer-file-name)
   " --- " p " -*- lexical-binding: t -*-" n n
   ";;; Commentary:" n n
   ";; " p n n
   ";;; Code:" n n p n n
   "(provide '" (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ")" n
   ";;; " (file-name-nondirectory buffer-file-name) " ends here")
