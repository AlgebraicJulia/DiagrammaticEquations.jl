using .decapodes


using PEG

# Dylan >

# Line := Statement & EOL
# Statement := Judgement | Eq
# Judgement := var::var
# var := ident | Expr
# Eq := term & "==" & term

# Christian >

# Term := derivative | function | operation  
# derivative = (∂ₜ | dt) Var
# function = ident (args)
# args = term | term, term
# operation = term ws? ((+|*) ws? term)+
# compose = ∘(args)(term)

@rule derivative = (∂ₜ , dt) & lparen & ident & rparen