if exists("b:current_syntax")
        finish
endif

syntax keyword potionKeyword let fn 
highlight link potionKeyword Keyword


syntax match potionComment "\v\d"
highlight link potionComment Number


syntax match potionOperator "\v\*"
syntax match potionOperator "\v/"
syntax match potionOperator "\v\+"
syntax match potionOperator "\v-"
syntax match potionOperator "\v\?"
syntax match potionOperator "\v\*\="
syntax match potionOperator "\v/\="
syntax match potionOperator "\v\+\="
syntax match potionOperator "\v-\="

highlight link potionOperator Operator

let b:current_syntax = "potion"
