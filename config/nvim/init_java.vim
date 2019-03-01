" ---------------------------------------------------------------------------
"    Java formatting
" ---------------------------------------------------------------------------
let g:clang_format#auto_format = 1
let g:clang_format#auto_format_on_insert_leave = 0

let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11",
            \
            \"BasedOnStyle": "LLVM",
            \"Language": "Java",
            \"IndentWidth": 8,
            \"UseTab": "Always",
            \"BreakBeforeBraces": "Linux",
            \"AlwaysBreakBeforeMultilineStrings": "true",
            \"AllowShortIfStatementsOnASingleLine": "false",
            \"AllowShortLoopsOnASingleLine": "false",
            \"AllowShortFunctionsOnASingleLine": "false",
            \"IndentCaseLabels": "false",
            \"AlignEscapedNewlinesLeft": "false",
            \"AlignTrailingComments": "true",
            \"AllowAllParametersOfDeclarationOnNextLine": "false",
            \"AlignAfterOpenBracket": "true",
            \"SpaceAfterCStyleCast": "false",
            \"MaxEmptyLinesToKeep": 2,
            \"BreakBeforeBinaryOperators": "NonAssignment",
            \"BreakStringLiterals": "false",
            \"SortIncludes":    "false",
            \"ContinuationIndentWidth": 8}

