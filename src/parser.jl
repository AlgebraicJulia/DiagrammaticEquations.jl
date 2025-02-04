import Catlab.Parsers.ParserCore: ident
using PEG

# Dylan >

# Line := Statement & EOL
# Statement := Judgement | Eq
# Judgement := var::var
# var := ident | Expr
# Eq := term & "==" & term

# Christian >

# Term := derivative | function | operation | Ident | Number 
# derivative = (∂ₜ | dt) Var
# function = ident (args)
# args = term | term, term
# operation = term ws? ((+|*) ws? term)+
# compose = ∘(args)(term)

# 
@rule Term = Derivative , Call, 
  ident |> v -> parse_identifier(v)

# The derivative rule supports derivatives of the form ∂ₜ(x) and dt(x).
@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & ident & ws & rparen |> v -> Tan(decapodes.Var(Symbol(v[4])))

# The call rule supports function calls of the form f(x) and g(x, y).
@rule Call = ident & lparen & ws & Args & ws & rparen

# Arguments support one or two terms.
@rule Args = Term[1:2]

