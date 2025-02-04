import Catlab.Parsers.ParserCore: ident
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

# The derivative rule supports derivatives of the form ∂ₜ(x) and dt(x).
@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & ident & ws & rparen |> v -> Tan(decapodes.Var(Symbol(v[4])))

