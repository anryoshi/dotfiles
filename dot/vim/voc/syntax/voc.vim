if exists("b:current_syntax")
    finish
endif

syntax match vocTerm "\v^[^|]*\ze\|"
syntax match vocDefinition "\v\|\zs.*$"

highlight link vocTerm Keyword
highlight link vocDefinition Label

let b:current_syntax = "voc"
