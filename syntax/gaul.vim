if exists('b:current_syntax')
  finish
endif

" Keywords
syn keyword gaulKeyword fn let var return if else while for break continue
syn keyword gaulKeyword import export from

" Booleans
syn keyword gaulBoolean true false

" Null
syn keyword gaulNull null

" Numbers — decimal with optional underscores
syn match gaulNumber '\<\d\+\%(_\d\+\)*\>'
" Hex
syn match gaulNumber '\<0[xX][0-9a-fA-F]\+\%(_[0-9a-fA-F]\+\)*\>'
" Binary
syn match gaulNumber '\<0[bB][01]\+\%(_[01]\+\)*\>'
" Octal
syn match gaulNumber '\<0[oO][0-7]\+\%(_[0-7]\+\)*\>'
" Floats
syn match gaulFloat '\<\d\+\%(_\d\+\)*\.\d\+\%(_\d\+\)*\>'

" Strings
syn region gaulString start='"' skip='\\"' end='"' contains=gaulEscape
syn match gaulEscape '\\[ntr"\\]' contained

" Comments
syn match gaulLineComment '//.*$'
syn region gaulBlockComment start='/\*' end='\*/' contains=gaulBlockComment

" Operators
syn match gaulOperator '\~='
syn match gaulOperator '\.\.'

" Highlight links
hi def link gaulKeyword     Keyword
hi def link gaulBoolean     Boolean
hi def link gaulNull        Constant
hi def link gaulNumber      Number
hi def link gaulFloat       Float
hi def link gaulString      String
hi def link gaulEscape      SpecialChar
hi def link gaulLineComment Comment
hi def link gaulBlockComment Comment
hi def link gaulOperator    Operator

let b:current_syntax = 'gaul'
