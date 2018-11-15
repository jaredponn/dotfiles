
" ---------------------------------------------------------------------------
"    YCMD
" ---------------------------------------------------------------------------
let g:ycm_semantic_triggers =  {
  \   'c' : ['->', '.', '_'],
  \   'cpp,cuda,objcpp' : ['->', '.', '::'],
  \ }

  "\   'haskell' : ['.']
"let g:ycm_semantic_triggers = {'haskell' : ['re!.']}
"let g:ycm_semantic_triggers.haskell = ['re![^ ]+']
" removes the annoying autocompeltiong menu
"let g:ycm_autoclose_preview_window_after_completion = 1
" removes the anonying copmletion mode after leaving instert mode
" let g:ycm_autoclose_preview_window_after_insertion = 1

" ---------------------------------------------------------------------------
"    C formatting
" ---------------------------------------------------------------------------
let g:clang_format#auto_format = 1
let g:clang_format#auto_format_on_insert_leave = 0

let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11",
            \
            \"BasedOnStyle": "LLVM",
            \"Language": "Cpp",
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

" Pretty comments
nnoremap <Leader>cd i// -----------------------------------------<CR>//    Sectional Dividers Like This<CR>// -----------------------------------------<CR><esc>kkwv$h
nnoremap <Leader>cf A  /**< field info */<esc>12hvee
nnoremap <Leader>ct cc/**<cr>*<cr>*/<esc>ka

" ---------------------------------------------------------------------------
"    Opening header /cpp file
" ---------------------------------------------------------------------------
"swap to header
nnoremap <Leader>sh :e %<.h<cr>
"swap to c
nnoremap <Leader>sc :e %<.c<cr>

nnoremap <Leader>vsh :vsplit %<.h<cr>
nnoremap <Leader>vsc :vsplit %<.c<cr>

" tags and joy"
nnoremap <Leader>t :Tags<cr>
nnoremap <Leader>bt :Tags<cr>
