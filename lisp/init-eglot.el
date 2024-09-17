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
               '(templ-ts-mode . ("templ" "lsp")))
  (add-to-list 'eglot-server-programs
               '(glsl-mode . ("glsl_analyzer")))

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
                                          :usePlaceholders t))))

  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

  (defun drsl/format-buffer ()
    "Format buffer according to major mode."
    (interactive)
    (cond ((eq major-mode #'c++-mode) (clang-format-buffer))
          (t (eglot-format-buffer))))

  ;; Load after format-all.
  (keymap-set eglot-mode-map "C-c e j" #'format-all-buffer)

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

(setq compile-command "make")

;; Find the nearest parent go.mod as the project root.
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


;; clang-format
(elpaca
    clang-format
  (require 'clang-format)

  (setq-default clang-format-fallback-style "llvm")
  (defun clang-format-buffer-on-save ()
    (add-hook 'before-save-hook #'clang-format-buffer -10 t)))


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
AlwaysBreakTemplateDeclarations: false
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
CFLAGS = $(INCS) -O2 -std=c18
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
CFLAGS = $(INCS) -O2 -std=c18
CXXFLAGS = $(INCS) -O2 -std=c++17
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

(require 'shapeless-c-arrow)
(add-hook 'c-mode-hook      #'shapeless-c-arrow-mode)
(add-hook 'c++-mode-hook    #'shapeless-c-arrow-mode)
(add-hook 'c-ts-mode-hook   #'shapeless-c-arrow-mode)
(add-hook 'c++-ts-mode-hook #'shapeless-c-arrow-mode)

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
   (let* ((original-string
           (treesit-node-text
            (treesit-parent-until (treesit-node-at (point))
                                  (lambda (NODE)
                                    (string-equal (treesit-node-type NODE)
                                                  "field_declaration")))
            t))
          (insert-string (concat (treesit-node-text
                                  (treesit-node-child-by-field-name
                                   (treesit-parent-until
                                    (treesit-node-at (point))
                                    (lambda (NODE)
                                      (let ((NODE-TYPE (treesit-node-type NODE)))
                                        (or (string-equal NODE-TYPE "class_specifier")
                                            (string-equal NODE-TYPE "struct_specifier")))
                                      )
                                    t)
                                   "name")
                                  t)
                                 "::"))
          (insert-pos (1+ (string-match " " original-string))))
     (replace-regexp-in-string ";" "{\n\n}"
                               (concat (substring original-string 0 insert-pos)
                                       insert-string
                                       (substring original-string insert-pos)))
     ))
  (message "definition is put in kill-ring"))

(provide 'init-eglot)
;;; init-eglot.el ends here
