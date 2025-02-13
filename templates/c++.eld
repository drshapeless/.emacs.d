c++-mode
c++-ts-mode

(cerr "std::cerr << " p q " << std::endl;")
(cf "// clang-format off" n p q n "// clang-format on" n)
(cout "std::cout << " p q " << std::endl;")
(endl "<< std::endl;")

;; Header
(h "#ifndef "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_HH" n
   "#define "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_HH" n n p q n n
   "#endif // "
   (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
   "_HH")

(main "int main(int argc, char *argv[]) {" n
      > p q n
      > "return 0;" n
      "}")

;; Runtime error
(rte "std::runtime_error(" p q ")")

(s "std::string")
(v "std::vector")

(todo
 > "// TODO: " p q)

(p "#pragma once")

(c "VK_CHECK(result);" n > )
(r "vk::Result result;" n > )
(tie "std::tie(result, " p q ")")
