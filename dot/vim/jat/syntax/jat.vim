if exists("b:current_syntax")
    finish
endif


syntax match jatLabel "\v\s\@[0-9a-zA-Z]+|^\@[0-9a-zA-Z]+"
highlight link jatLabel Identifier

syntax match jatTodoState "\v*" contained
syntax match jatTodoItem "\v\[.\]" contains=jatTodoState
highlight link jatTodoState Green
highlight link jatTodoItem Todo

syntax match jatHeaderString "\v\s.*" contained contains=jatLabel
syntax region jatHeader start="\v#\d" end="$" contains=jatHeaderString
highlight link jatHeaderString String
highlight link jatHeader Keyword

let b:current_syntax = "jat"
