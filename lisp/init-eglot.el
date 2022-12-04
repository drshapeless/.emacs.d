;;; init-eglot.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; Eglot is the only decent lsp server solution in Emacs.

;; Eglot now comes with Emacs.

;;; Code:

(if *is-older-emacs*
    (straight-use-package 'eglot))
(require 'eglot)

;; This stops eglot from logging the json events of lsp server.
(setq eglot-events-buffer-size 0)
;; Do not show multiline eldoc.
(setq eldoc-echo-area-use-multiline-p nil)

;; Auto shutdown server
(setq eglot-autoshutdown t)
;; Auto shutdown somehow causes some weird lag.

(add-hook 'c-mode-hook          #'eglot-ensure)
(add-hook 'c++-mode-hook        #'eglot-ensure)
(add-hook 'objc-mode-hook       #'eglot-ensure)
(add-hook 'swift-mode-hook      #'eglot-ensure)
(add-hook 'python-mode-hook     #'eglot-ensure)
(add-hook 'js-mode-hook         #'eglot-ensure)
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'go-mode-hook         #'eglot-ensure)
(add-hook 'sql-mode-hook        #'eglot-ensure)
(add-hook 'dart-mode-hook       #'eglot-ensure)
(add-hook 'rustic-mode-hook     #'eglot-ensure)
(add-hook 'svelte-mode-hook     #'eglot-ensure)
(add-hook 'html-mode-hook       #'eglot-ensure)

(keymap-set eglot-mode-map "C-c e r" #'eglot-reconnect)
(keymap-set eglot-mode-map "C-c e f" #'eglot-code-action-quickfix)
(keymap-set eglot-mode-map "C-c e n" #'eglot-rename)

(add-to-list 'eglot-server-programs
             '(c-mode . ("clangd" "--header-insertion=never")))
(add-to-list 'eglot-server-programs
             '(c++-mode . ("clangd" "--header-insertion=never")))
(add-to-list 'eglot-server-programs
             '(swift-mode . ("sourcekit-lsp")))
(add-to-list 'eglot-server-programs
             '(sql-mode . ("sqls")))
;; (add-to-list 'eglot-server-programs
;;              '(dart-mode . ("dart" "language-server")))
(add-to-list 'eglot-server-programs
             '(svelte-mode . ("svelteserver" "--stdio")))

;; Deno support from https://deno.land/manual@v1.28.3/getting_started/setup_your_environment
(add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list :enable t
        :lint t))

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
(c-add-style "llvm"
             '("gnu"
               (fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)
                                   (statement-cont . llvm-lineup-statement)))))

(defun drsl/use-c-linux-cc-llvm-style ()
  (interactive)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-mode . "linux")
                          (cc-mode . "llvm"))))

;; Use linux style on C/C++ file
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")
                        (cc-mode . "linux")
                        (other . "llvm")))

(setq compile-command "make")

;; Find the nearest parent go.mod as the project root.
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; LSP settings.
(setq-default eglot-workspace-configuration
              '(;; gopls config.
                (:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))
                ;; dart config.
                (:dart .
                       ((completeFunctionCalls . t)))))

;; clang-format
(let ((clang-format-path "/usr/share/clang/clang-format.el"))
  (if *is-a-mac*
        (setq clang-format-path (concat (getenv "HOMEBREW_PREFIX")
                                        "/share/clang/clang-format.el")))
  (if (file-exists-p clang-format-path)
      (load clang-format-path)
    (message "clang-format not found")))

(require 'clang-format)

(setq-default clang-format-fallback-style "llvm")

(defun drsl/generate-clang-format ()
  "Generate a .clang-format file in the current directory."
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

(defun clang-format-buffer-on-save ()
  (add-hook 'before-save-hook #'clang-format-buffer -10 t))

;; (add-hook 'c-mode-hook #'clang-format-buffer-on-save)
;; (add-hook 'c++-mode-hook #'clang-format-buffer-on-save)

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
;; (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
;; (add-hook 'svelte-mode-hook #'eglot-format-buffer-on-save)

;; (add-hook 'dart-mode-hook #'eglot-format-buffer-on-save)

(defun drsl/format-buffer ()
  "Format buffer according to major mode."
  (interactive)
  (cond ((eq major-mode #'c++-mode) (clang-format-buffer))
        (t (eglot-format-buffer))))

;; Load after format-all.
(keymap-set eglot-mode-map "C-c e j" #'format-all-buffer)

(keymap-set eglot-mode-map "C-c e o" #'ff-find-other-file-other-window)
(keymap-set eglot-mode-map "C-c e p" #'ff-find-other-file)

(require 'shapeless-c-arrow)
(add-hook 'c-mode-hook #'shapeless-c-arrow-mode)
(add-hook 'c++-mode-hook #'shapeless-c-arrow-mode)

(provide 'init-eglot)
;;; init-eglot.el ends here
