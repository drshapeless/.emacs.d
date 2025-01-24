c-mode
c-ts-mode

(cf "/* clang-format off */" n
    p q n
    "/* clang-format on */")

;; Header
(h "#ifndef "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_H" n
   "#define "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_H" n n p q n n
   "#endif /* "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_H */")

(main "int main(int argc, char *argv[]) {" n
      > p q n
      > "return 0;" n
      "}")

(objc "/* -*- mode: objc -*- */")

(todo "/* TODO: " p q " */")

(cpp "#ifdef __cplusplus" n
     "extern \"C\" {" n
     "#endif" n n r
     "#ifdef __cplusplus" n
     "}" n
     "#endif" n)

(e "ERROR(\"" p q "\");" n
>   "exit(EXIT_FAILURE);" n)

(s "struct ")

(p "#pragma once")

(da "/* darray */")
