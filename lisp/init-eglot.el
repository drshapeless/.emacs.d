;;; init-eglot.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Eglot is the only decent lsp server solution in Emacs.

;; Eglot now comes with Emacs.

;;; Code:

(elpaca eldoc
  (require 'eldoc))

(elpaca jsonrpc
  (require 'jsonrpc))

(elpaca eglot
  (require 'eglot)
  ;; This stops eglot from logging the json events of lsp server.
  ;; (setq eglot-events-buffer-size 0)
  ;; Do not show multiline eldoc.
  ;; (setq eldoc-echo-area-use-multiline-p nil)

  ;; Auto shutdown server
  (setq eglot-autoshutdown t)
  ;; Auto shutdown somehow causes some weird lag.

  (add-hook 'c-ts-mode-hook          #'eglot-ensure)
  (add-hook 'c++-ts-mode-hook        #'eglot-ensure)
  (add-hook 'python-ts-mode-hook     #'eglot-ensure)
  (add-hook 'js-ts-mode-hook         #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook         #'eglot-ensure)
  (add-hook 'cmake-ts-mode-hook      #'eglot-ensure)
  (add-hook 'swift-ts-mode-hook      #'eglot-ensure)
  (add-hook 'c3-ts-mode-hook         #'eglot-ensure)
  (add-hook 'odin-ts-mode-hook       #'eglot-ensure)
  (add-hook 'zig-ts-mode-hook        #'eglot-ensure)
  (add-hook 'glsl-ts-mode-hook       #'eglot-ensure)

  (add-hook 'glsl-mode-hook       #'eglot-ensure)
  (add-hook 'objc-mode-hook       #'eglot-ensure)
  (add-hook 'sql-mode-hook        #'eglot-ensure)
  (add-hook 'dart-mode-hook       #'eglot-ensure)
  (add-hook 'rustic-mode-hook     #'eglot-ensure)
  (add-hook 'svelte-mode-hook     #'eglot-ensure)
  (add-hook 'html-mode-hook       #'eglot-ensure)
  (add-hook 'tmpl-mode-hook       #'eglot-ensure)
  (add-hook 'templ-ts-mode-hook   #'eglot-ensure)
  (add-hook 'web-mode-hook        #'eglot-ensure)
  (add-hook 'zig-mode-hook        #'eglot-ensure)

  (keymap-set eglot-mode-map "C-c e a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c e r" #'eglot)
  (keymap-set eglot-mode-map "C-c e f" #'eglot-code-action-quickfix)
  (keymap-set eglot-mode-map "C-c e n" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c e h" #'eglot-inlay-hints-mode)

  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("clangd" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(c-ts-mode . ("clangd" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(c++-ts-mode . ("clangd" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(swift-ts-mode . ("sourcekit-lsp")))
  (add-to-list 'eglot-server-programs
               '(sql-mode . ("sqls")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(dart-mode . ("dart" "language-server")))
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(tmpl-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(web-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(templ-ts-mode . ("lspx" "--lsp" "templ lsp" "--lsp" "tailwindcss-language-server --stdio" "--lsp" "vscode-html-language-server --stdio")))
  (add-to-list 'eglot-server-programs
               '(glsl-mode . ("glsl_analyzer")))
  (add-to-list 'eglot-server-programs
               '(c3-ts-mode . ("c3lsp")))
  (add-to-list 'eglot-server-programs
               '(glsl-ts-mode . ("glsl_analyzer")))

  ;; Deno support from https://deno.land/manual@v1.28.3/getting_started/setup_your_environment
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t))

  ;; LSP settings.
  (setq-default eglot-workspace-configuration
                (list (cons :gopls  (list :staticcheck t
                                          :matcher "CaseSensitive"
                                          :hints (list :assignVariableTypes t)
                                          :usePlaceholders t))
                      (cons :tailwindCSS  (list :includeLanguages (list :templ "html")
                                                ))))

  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

  (defun drsl/format-buffer ()
    "Format buffer according to major mode."
    (interactive)
    (cond ((eq major-mode #'c++-mode) (clang-format-buffer))
          (t (eglot-format-buffer))))

  ;; Load after format-all.
  (keymap-set eglot-mode-map "C-c e j" #'drsl/format-buffer)

  (keymap-set eglot-mode-map "C-c e o" #'ff-find-other-file-other-window)
  (keymap-set eglot-mode-map "C-c e p" #'ff-find-other-file)

;;; Things with Corfu
  ;; Use Eglot to provide continuously updated candidates
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defcustom drsl/eglot-extra-completion-functions '()
    "extra completion functions for eglot"
    :type '(repeat string))

  (defun drsl/eglot-capf ()
    (mapc
     (lambda (FUNCTION)
       (add-to-list 'completion-at-point-functions
                    FUNCTION))
     drsl/eglot-extra-completion-functions))

  (add-hook 'eglot-managed-mode-hook #'drsl/eglot-capf)
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

  ;; https://github.com/joaotavora/eglot/issues/574
  ;; This is for gopls to remove unused imports.
  (defun eglot-organize-imports-on-save ()
    (add-hook 'before-save-hook
              (lambda ()
                (call-interactively 'eglot-code-action-organize-imports))
              nil t))
  (add-hook 'go-ts-mode-hook #'eglot-organize-imports-on-save))

(defun eglot-open-link ()
  "Open markdown link at point in the `eldoc-doc-buffer'."
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
    (if url
        (browse-url url)
      (message "No URL found at point"))))

;; git clone https://github.com/blahgeek/emacs-lsp-booster
;; cd emacs-lsp-booster && cargo install --path .
(elpaca (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  (require 'eglot-booster)
  (eglot-booster-mode)
  )


(setq-default c-basic-offset 4)
(setq c-ts-mode-indent-offset 4)


(defun drsl/use-c-gnu-style ()
  (interactive)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "gnu"))))

;; This is stolen from llvm.
(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code
;; (c-add-style "llvm"
;;              '("gnu"
;;                (fill-column . 80)
;;                (c++-indent-level . 2)
;;                (c-basic-offset . 2)
;;                (indent-tabs-mode . nil)
;;                (c-offsets-alist . ((arglist-intro . ++)
;;                                    (innamespace . 0)
;;                                    (member-init-intro . ++)
;;                                    (statement-cont . llvm-lineup-statement)))))

(defun drsl/use-c-linux-cc-llvm-style ()
  (interactive)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "linux")
                          (cc-mode . "llvm"))))

;; Use linux style on C/C++ file
;; (setq c-default-style '((java-mode . "java")
;;                         (awk-mode . "awk")
;;                         (c-mode . "linux")
;;                         (cc-mode . "linux")
;;                         (other . "llvm")))


(defun drsl/c-ts-indent-style()
  "Override the built-in BSD indentation style with some additional rules.
         Docs:
         https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
         Notes: `treesit-explore-mode' can be very useful to see where
         you're at in the tree-sitter tree, especially paired
         with `(setq treesit--indent-verbose t)' to debug what rules is
         being applied at a given point."
  `(;; do not indent preprocessor statements
    ((node-is "preproc") column-0 0)
    ;; do not indent namespace children
    ((n-p-gp nil nil "namespace_definition") grand-parent 0)
    ;; append to k&r style
    ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: figure out what happened and fix this
(defun drsl/c-ts-add-indent-style ()
  "Directly add indent styles into `treesit-simple-indent-rules'

This is a ridiculous temporary fix in latest Emacs where
`c-ts-mode--indent-styles' is gone."
  (let ((my-rules treesit-simple-indent-rules)
        (rule-to-add '((n-p-gp nil nil "namespace_definition") grand-parent 0)))
    (setq-local treesit-simple-indent-rules
                (list (cons (car (car my-rules))
                            (cons rule-to-add
                                  (cdr (car my-rules))))))))

(if (version< emacs-version "30.0")
    (setq c-ts-mode-indent-style #'drsl/c-ts-indent-style)
  (progn (setq c-ts-mode-indent-style 'k&r)
         (add-hook 'c++-ts-mode-hook #'drsl/c-ts-add-indent-style)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq compile-command "make")

;; clang-format
(elpaca
    clang-format
  (require 'clang-format)

  (setq-default clang-format-fallback-style "llvm")
  (defun clang-format-buffer-on-save ()
    (add-hook 'before-save-hook #'clang-format-buffer -10 t)))

(defun drsl/generate-clangd-config ()
  "Generate a .clangd file in the current directory.

Default to locate compile_commands.json in the build directory."
  (interactive)
  (write-region
   "CompileFlags:
\tCompilationDatabase: build/"
   nil (if (eq major-mode 'dired-mode)
           (concat (dired-current-directory)
                   ".clangd")
         (concat (file-name-directory (buffer-file-name))
                 ".clangd"))))

(defun drsl/generate-clang-format ()
  "Generate a .clang-format file in the current directory.

This is my own style.
4 space indentation.
curly brace at the same line."
  (interactive)
  (write-region "---
AccessModifierOffset: -4
AlignAfterOpenBracket: Align
AlignConsecutiveAssignments: false
AlignConsecutiveDeclarations: false
AlignEscapedNewlines: Left
AlignOperands: true
AlignTrailingComments: false
AllowAllParametersOfDeclarationOnNextLine: false
AllowShortBlocksOnASingleLine: false
AllowShortCaseLabelsOnASingleLine: false
AllowShortFunctionsOnASingleLine: None
AllowShortIfStatementsOnASingleLine: false
AllowShortLoopsOnASingleLine: false
AlwaysBreakAfterDefinitionReturnType: None
AlwaysBreakAfterReturnType: None
AlwaysBreakBeforeMultilineStrings: false
AlwaysBreakTemplateDeclarations: true
BinPackArguments: false
BinPackParameters: false
BraceWrapping:
  AfterClass: false
  AfterControlStatement: false
  AfterEnum: false
  AfterFunction: false
  AfterNamespace: false
  AfterObjCDeclaration: false
  AfterStruct: false
  AfterUnion: false
  AfterExternBlock: false
  BeforeCatch: false
  BeforeElse: false
  IndentBraces: false
  SplitEmptyFunction: true
  SplitEmptyRecord: true
  SplitEmptyNamespace: true
BreakBeforeBinaryOperators: None
BreakBeforeBraces: Custom
BreakBeforeInheritanceComma: false
BreakBeforeTernaryOperators: false
BreakConstructorInitializersBeforeComma: false
BreakConstructorInitializers: BeforeComma
BreakAfterJavaFieldAnnotations: false
BreakStringLiterals: false
ColumnLimit: 80
CommentPragmas: '^ IWYU pragma:'
CompactNamespaces: false
ConstructorInitializerAllOnOneLineOrOnePerLine: false
ConstructorInitializerIndentWidth: 4
ContinuationIndentWidth: 4
Cpp11BracedListStyle: false
DerivePointerAlignment: false
DisableFormat: false
ExperimentalAutoDetectBinPacking: false
FixNamespaceComments: false

IncludeBlocks: Preserve
IncludeCategories:
  - Regex: '.*'
    Priority: 1
IncludeIsMainRegex: '(Test)?$'
IndentCaseLabels: false
IndentGotoLabels: false
IndentPPDirectives: None
IndentWidth: 4
IndentWrappedFunctionNames: false
JavaScriptQuotes: Leave
JavaScriptWrapImports: true
KeepEmptyLinesAtTheStartOfBlocks: false
MacroBlockBegin: ''
MacroBlockEnd: ''
MaxEmptyLinesToKeep: 1
NamespaceIndentation: None
ObjCBinPackProtocolList: Auto
ObjCBlockIndentWidth: 4
ObjCSpaceAfterProperty: true
ObjCSpaceBeforeProtocolList: true

# Taken from git's rules
PenaltyBreakAssignment: 10
PenaltyBreakBeforeFirstCallParameter: 30
PenaltyBreakComment: 10
PenaltyBreakFirstLessLess: 0
PenaltyBreakString: 10
PenaltyExcessCharacter: 100
PenaltyReturnTypeOnItsOwnLine: 60

PointerAlignment: Right
ReflowComments: false
SortIncludes: false
SortUsingDeclarations: false
SpaceAfterCStyleCast: false
SpaceAfterTemplateKeyword: true
SpaceBeforeAssignmentOperators: true
SpaceBeforeCtorInitializerColon: true
SpaceBeforeInheritanceColon: true
SpaceBeforeParens: ControlStatementsExceptForEachMacros
SpaceBeforeRangeBasedForLoopColon: true
SpaceInEmptyParentheses: false
SpacesBeforeTrailingComments: 1
SpacesInAngles: false
SpacesInContainerLiterals: false
SpacesInCStyleCastParentheses: false
SpacesInParentheses: false
SpacesInSquareBrackets: false
Standard: Cpp03
TabWidth: 4
UseTab: Never
...
" nil (if (eq major-mode 'dired-mode)
          (concat (dired-current-directory)
                  ".clang-format")
        (concat (file-name-directory (buffer-file-name))
                ".clang-format"))))

(defun drsl/generate-linux-style-clang-format ()
  "Generate a .clang-format file in the current directory.

This is the linux-style one. Use tab over space."
  (interactive)
  (write-region "---
AccessModifierOffset: -4
AlignAfterOpenBracket: Align
AlignConsecutiveAssignments: false
AlignConsecutiveDeclarations: false
AlignEscapedNewlines: Left
AlignOperands: true
AlignTrailingComments: false
AllowAllParametersOfDeclarationOnNextLine: false
AllowShortBlocksOnASingleLine: false
AllowShortCaseLabelsOnASingleLine: false
AllowShortFunctionsOnASingleLine: None
AllowShortIfStatementsOnASingleLine: false
AllowShortLoopsOnASingleLine: false
AlwaysBreakAfterDefinitionReturnType: None
AlwaysBreakAfterReturnType: None
AlwaysBreakBeforeMultilineStrings: false
AlwaysBreakTemplateDeclarations: false
BinPackArguments: true
BinPackParameters: true
BraceWrapping:
  AfterClass: false
  AfterControlStatement: false
  AfterEnum: false
  AfterFunction: true
  AfterNamespace: true
  AfterObjCDeclaration: false
  AfterStruct: false
  AfterUnion: false
  AfterExternBlock: false
  BeforeCatch: false
  BeforeElse: false
  IndentBraces: false
  SplitEmptyFunction: true
  SplitEmptyRecord: true
  SplitEmptyNamespace: true
BreakBeforeBinaryOperators: None
BreakBeforeBraces: Custom
BreakBeforeInheritanceComma: false
BreakBeforeTernaryOperators: false
BreakConstructorInitializersBeforeComma: false
BreakConstructorInitializers: BeforeComma
BreakAfterJavaFieldAnnotations: false
BreakStringLiterals: false
ColumnLimit: 80
CommentPragmas: '^ IWYU pragma:'
CompactNamespaces: false
ConstructorInitializerAllOnOneLineOrOnePerLine: false
ConstructorInitializerIndentWidth: 8
ContinuationIndentWidth: 8
Cpp11BracedListStyle: false
DerivePointerAlignment: false
DisableFormat: false
ExperimentalAutoDetectBinPacking: false
FixNamespaceComments: false

IncludeBlocks: Preserve
IncludeCategories:
  - Regex: '.*'
    Priority: 1
IncludeIsMainRegex: '(Test)?$'
IndentCaseLabels: false
IndentGotoLabels: false
IndentPPDirectives: None
IndentWidth: 8
IndentWrappedFunctionNames: false
JavaScriptQuotes: Leave
JavaScriptWrapImports: true
KeepEmptyLinesAtTheStartOfBlocks: false
MacroBlockBegin: ''
MacroBlockEnd: ''
MaxEmptyLinesToKeep: 1
NamespaceIndentation: None
ObjCBinPackProtocolList: Auto
ObjCBlockIndentWidth: 8
ObjCSpaceAfterProperty: true
ObjCSpaceBeforeProtocolList: true

# Taken from git's rules
PenaltyBreakAssignment: 10
PenaltyBreakBeforeFirstCallParameter: 30
PenaltyBreakComment: 10
PenaltyBreakFirstLessLess: 0
PenaltyBreakString: 10
PenaltyExcessCharacter: 100
PenaltyReturnTypeOnItsOwnLine: 60

PointerAlignment: Right
ReflowComments: false
SortIncludes: false
SortUsingDeclarations: false
SpaceAfterCStyleCast: false
SpaceAfterTemplateKeyword: true
SpaceBeforeAssignmentOperators: true
SpaceBeforeCtorInitializerColon: true
SpaceBeforeInheritanceColon: true
SpaceBeforeParens: ControlStatementsExceptForEachMacros
SpaceBeforeRangeBasedForLoopColon: true
SpaceInEmptyParentheses: false
SpacesBeforeTrailingComments: 1
SpacesInAngles: false
SpacesInContainerLiterals: false
SpacesInCStyleCastParentheses: false
SpacesInParentheses: false
SpacesInSquareBrackets: false
Standard: Cpp03
TabWidth: 8
UseTab: Always
...
" nil (if (eq major-mode 'dired-mode)
          (concat (dired-current-directory)
                  ".clang-format")
        (concat (file-name-directory (buffer-file-name))
                ".clang-format"))))

(defun drsl/generate-makefile ()
  "Generate makefile"
  (interactive)
  (write-region "BIN =

# compiler
CC = clang

# includes and libs
INCS =
LIBS =

# flags
CFLAGS = $(INCS) -O2 -std=c23
LDFLAGS = $(LIBS)
" nil (if (eq major-mode 'dired-mode)
          (concat (dired-current-directory)
                  "config.mk")
        (concat (file-name-directory (buffer-file-name))
                "config.mk")))
  (write-region "include config.mk

SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)

all: $(BIN)

$(BIN): $(OBJ)
\t$(CC) -o $@ $(OBJ) $(LDFLAGS)

$(filter-out main.o, $(OBJ)): %.o: %.h

clean:
\trm -f $(BIN) $(OBJ)

run: $(BIN)
\t./$(BIN)

.PHONY: all clean run
" nil (if (eq major-mode 'dired-mode)
          (concat (dired-current-directory)
                  "makefile")
        (concat (file-name-directory (buffer-file-name))
                "makefile"))))

(defun drsl/generate-cpp-makefile ()
  "Generate makefile"
  (interactive)
  (write-region "BIN =

# compiler
CC = clang
CXX = clang++

# includes and libs
INCS =
LIBS =

# flags
CFLAGS = $(INCS) -O2 -std=c23
CXXFLAGS = $(INCS) -O2 -std=c++20
LDFLAGS = $(LIBS)
" nil (if (eq major-mode 'dired-mode)
          (concat (dired-current-directory)
                  "config.mk")
        (concat (file-name-directory (buffer-file-name))
                "config.mk")))
  (write-region "include config.mk

CSRC = $(wildcard *.c)
CXXSRC = $(wildcard *.cc)
COBJ = $(CSRC:.c=.o)
CXXOBJ = $(CXXSRC:.cc=.o)
OBJ = $(COBJ) $(CXXOBJ)

all: $(BIN)

$(BIN): $(OBJ)
\t$(CXX) -o $@ $(OBJ) $(LDFLAGS)

$(filter-out main.o, $(CXXOBJ)): %.o: %.hh

$(COBJ): %.o: %.h

clean:
\trm -f $(BIN) $(OBJ)

run: $(BIN)
\t./$(BIN)

.PHONY: all clean run
" nil (if (eq major-mode 'dired-mode)
          (concat (dired-current-directory)
                  "makefile")
        (concat (file-name-directory (buffer-file-name))
                "makefile"))))

(defun drsl/generate-prettierrc ()
  "Generate a .prettierrc.toml in the current directory."
  (interactive)
  (write-region "useTabs = false
singleQuote = false
semi = true
trailingComma = \"es5\"
printWidth = 80
pluginSearchDirs = [\".\"]
overrides = [ { files = \"*.svelte\", options = { parser = \"svelte\"}}]
"
                nil (if (eq major-mode 'dired-mode)
                        (concat (dired-current-directory)
                                ".prettierrc.toml")
                      (concat (file-name-directory (buffer-file-name))
                              ".prettierrc.toml"))))

(require 'shapeless-prog-mode-helper)
(add-hook 'c-mode-hook       #'shapeless-prog-mode)
(add-hook 'c++-mode-hook     #'shapeless-prog-mode)
(add-hook 'c-ts-mode-hook    #'shapeless-prog-mode)
(add-hook 'c++-ts-mode-hook  #'shapeless-prog-mode)
(add-hook 'odin-ts-mode-hook #'shapeless-prog-mode)


(elpaca
    (breadcrumb :host github :repo "joaotavora/breadcrumb")
  (require 'breadcrumb)
  (breadcrumb-mode))

(require 'cmake-ts-mode)

;; This is a cpp helper function to easily create definition from declaration
(defun drsl/cpp-definition-in-class ()
  "Format the current declaration of function into definition and
put it into kill-ring."
  (interactive)
  (kill-new
   (string-replace " override "
                   " "
                   (drsl/generate-cpp-class-function-definition-at-point)))
  (message "definition is put in kill-ring"))

(defun drsl/get-cpp-template-node (class-node)
  "Return parent template treesit node.

Return nil if is not in a template."
  (treesit-parent-until class-node
                        (lambda (NODE)
                          (string-equal (treesit-node-type NODE)
                                        "template_declaration"))))

(defun drsl/get-class-function-node-at-point ()
  "Return a treesit node of the current class function."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (NODE)
                          (string-equal (treesit-node-type NODE)
                                        "field_declaration"))
                        t))

(defun drsl/get-cpp-class-node-at-point ()
  "Return the current class treesit node."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (NODE)
                          (let ((NODE-TYPE (treesit-node-type NODE)))
                            (or (string-equal NODE-TYPE
                                              "class_specifier")
                                (string-equal NODE-TYPE
                                              "struct_specifier"))))
                        t))

(defun drsl/generate-cpp-class-function-definition-at-point ()
  "Return the class function definition at point."
  (interactive)
  (string-replace
   ";"
   " {\n\n}"
   (let* ((class-node (drsl/get-cpp-class-node-at-point))
          (func-node  (drsl/get-class-function-node-at-point))
          (template-node (drsl/get-cpp-template-node class-node))
          (class-text (treesit-node-text
                       (treesit-node-child-by-field-name
                        class-node
                        "name")
                       t))
          (func-text (treesit-node-text
                      func-node
                      t))
          (first-space-pos (string-match " "
                                         func-text))
          (insert-pos (string-match "[a-z]"
                                    func-text
                                    first-space-pos))         )
     (if template-node
         (let* ((template-parameter (treesit-node-text
                                     (treesit-node-child-by-field-name
                                      template-node
                                      "parameters")
                                     t))
                (template-head (concat "template "
                                       template-parameter
                                       "\n"))

                )
           (concat template-head
                   (substring func-text 0 insert-pos)
                   class-text
                   (string-replace "typename " "" template-parameter)
                   "::"
                   (substring func-text insert-pos))
           )

       (concat (substring func-text 0 insert-pos)
               class-text
               "::"
               (substring func-text insert-pos))))))

(defun drsl/get-function-node-at-point ()
  "Return a treesit node of the current class function."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (NODE)
                          (string-equal (treesit-node-type NODE)
                                        "declaration"))
                        t))

(defun drsl/generate-c-function-definition-at-point ()
  "Return the function definition at point"
  (interactive)
  (string-replace
   ";"
   " {\n\n}"
   (let* ((func-node (drsl/get-function-node-at-point))
          (func-text (treesit-node-text func-node t)))
     func-text)))

(defun drsl/c-function-definition ()
  "Format the current declaration of function into definition and
put it into kill-ring."
  (interactive)
  (kill-new
   (drsl/generate-c-function-definition-at-point))
  (message "definition is put in kill-ring"))

(defun drsl/c-expand-struct-pointer ()
  "Expand the current word into a struct pointer."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (progn
      (delete-region (beginning-of-thing 'word) (end-of-thing 'word))
      (insert "struct " word " *" (string-inflection-underscore-function word)))))

(provide 'init-eglot)
;;; init-eglot.el ends here
