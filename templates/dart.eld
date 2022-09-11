dart-mode

(wb
 > "@override" n
 > "Widget build(BuildContext context) {" n
 > "return " p ";" n
 "}")

(kk "({Key? key" p "}) : super(key: key);")

(main "void main() {" n
      > p n
      "}")

(mat "import 'package:flutter/material.dart';")

(@o "@override")

(todo "// TODO: " p)
