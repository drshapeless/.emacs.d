c-mode

(cf "/* clang-format off */" n
    p n
    "/* clang-format on */")

;; Header
(h "#ifndef "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_H" n
   "#define "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_H" n n p n n
   "#endif /* "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_H */")

(main "int main(int argc, char *argv[])" n
      "{" n
      > p n
      > "return 0;" n
      "}")

(objc "/* -*- mode: objc -*- */")

(todo "/* TODO: " p " */")
