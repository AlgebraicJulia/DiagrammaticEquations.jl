@intertypes "decapodes.it" module decapodes end
using .decapodes

import PEG

# Line := Statement & EOL
# Statement := Judgement | Eq
# Judgement := var::var
# var := Symbol | Expr
# Eq := term & "==" & term
# Term := derivative | function | operation  
# derivative = (∂ₜ | dt) Var
# function = ident (args)
# args = term | term, term
# operation = term ws? ((+|*) ws? term)+
# compose = ∘(args)(term)

