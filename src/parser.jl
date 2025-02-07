using PEG
import Catlab.Parsers.ParserCore: ident

# Dylan >

# Line := Statement & EOL
# Statement := Judgement | Eq
# Judgement := var::var
# var := ident | Expr
# Eq := term & "==" & term

# Christian >

# Term := derivative | call | operation | Ident | Number 
# derivative = (∂ₜ | dt) Var
# call = ident (args)
# args = term | term, term
# operation = term ws? ((+|*) ws? term)+
# compose = ∘(args)(term) NEED TO IMPLEMENT

# Terms make up core components of DEC equations. They can be symbols, numbers, arithmetic operations, derivatives, or function calls.
@rule Term = Derivative, 
  Call, 
  ident |> v -> ParseIdent(v),
  PlusOperation,
  MultOperation

# The derivative rule supports derivatives of the form ∂ₜ(x) and dt(x).
@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & ident & ws & rparen |> v -> Tan(decapodes.Var(Symbol(v[4])))

# The composition rule supports the compostion of terms A over term b.
@rule Compose = "∘" & lparen & ws & List & rparen & ws & lparen & ws & Term & rparen |> v -> AppCirc1(v[4], v[9])
@rule List = ident & (ws & comma & ws & ident)[*] |> v -> vcat(Symbol(v[1]), Symbol.(last.(v[2])))

# The operation rule supports addition and multiplication of terms.
@rule PlusOperation = Term & (ws & "+" & ws & Term)[+] |> v -> Plus(vcat(v[1], last.(v[2])))
@rule MultOperation = Term & (ws & "*" & ws & Term)[+] |> v -> Mult(vcat(v[1], last.(v[2])))

# The call rule supports function calls of the form f(x) and g(x, y).
@rule Call = ident & lparen & ws & Args & ws & rparen |> v -> BuildCall(v)

# Arguments support one or two terms.
@rule Args = (Term & ws & "," & ws & Term) |> v -> [v[1], v[5]], Term |> v -> [v]


""" BuildCall

Takes in an input array (AST) for a call expression and returns a corresponding App1 or App2 object.
"""
function BuildCall(v)
  if length(v[4]) == 1
    return App1(Symbol(v[1]), v[4][1])
  else
    return App2(Symbol(v[1]), v[4][1], v[4][2])
  end
end

""" ParseIdent

Takes in an input array (AST) for an identifier and returns a corresponding Var or Lit object.
"""
function ParseIdent(v)
  if typeof(Catlab.Parsers.ParserCore.parse_identifier(v)) == Symbol
    return decapodes.Var(Symbol(v))
  else
    return Lit(Symbol(v))
  end
end