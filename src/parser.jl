using PEG
import Catlab.Parsers.ParserCore: ident

# Dylan >

# Line := Statement & EOL
# Statement := Judgement | Eq
# Judgement := var::var
# var := ident | expr
# Eq := term & "==" & term

# Christian >

# Term := derivative | call | operation | Ident | Number 
# derivative = (∂ₜ | dt) Var
# call = ident (args)
# args = term | term, term
# operation = term ws? ((+|*) ws? term)+
# compose = ∘(args)(term) NEED TO IMPLEMENT

# Lines are made up of a statement followed by an end of line character. 
@rule Line = ws & Statement & r"[^\S\r\n]*" & EOL |> v->v[2]

# Statements can incldue either type judgements or equations.
@rule Statement = Judgement , Equation

# A judgement is a statement of the form A::B. It marks a type assignment.
@rule Judgement = (ident , (lparen & ws & List & ws & rparen)) & "::" & TypeName |> v -> BuildJudgement(v)
@rule TypeName = ident & ("{" & ident & "}")[:?] |> v -> BuildTypeName(v)

@rule Equation = Term & ws & "==" & ws & Term |> v -> Eq(v[1], v[5]) 

# Terms make up core components of DEC equations. They can be symbols, numbers, arithmetic operations, derivatives, or function calls.
# Term rule has major isses with precedence. Need to fix this.
@rule Term = Derivative, 
  Call, 
  ident |> v -> ParseIdent(v),
  PlusOperation,
  MultOperation,
  Compose

# The derivative rule supports derivatives of the form ∂ₜ(x) and dt(x).
@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & ident & ws & rparen |> v -> Tan(decapodes.Var(Symbol(v[4])))

# The composition rule supports the compostion of terms A over term b.
@rule Compose = "∘" & lparen & ws & List & rparen & ws & lparen & ws & Term & rparen |> v -> AppCirc1(v[4], v[9])

# The operation rule supports addition and multiplication of terms.
@rule PlusOperation = Term & (ws & "+" & ws & Term)[+] |> v -> Plus(vcat(v[1], last.(v[2])))
@rule MultOperation = Term & (ws & "*" & ws & Term)[+] |> v -> Mult(vcat(v[1], last.(v[2])))

# The call rule supports function calls of the form f(x) and g(x, y).
@rule Call = ident & lparen & ws & Args & ws & rparen |> v -> BuildCall(v)

# Arguments support one or two terms.
@rule Args = (Term & ws & "," & ws & Term) |> v -> [v[1], v[5]], Term |> v -> [v]
@rule List = ident & (ws & comma & ws & ident)[*] |> v -> vcat(Symbol(v[1]), Symbol.(last.(v[2])))

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

""" BuildJudgement

Takes in an input array (AST) for a Judgement corresponding Judgement object
"""
function BuildJudgement(v)
  pattern = (v[1], v[3])
  print("Pattern  = $pattern")
  @match pattern begin
    ([a...], [b...]) => map(sym -> Judgement(sym, Symbol(b[1]), Symbol(b[2])), Symbol.(a[3]))
    ([a...], b)       => map(sym -> Judgement(sym, Symbol(b), :I), Symbol.(a[3]))
    (a, [b...])       => Judgement(Symbol(a), Symbol(b[1]), Symbol(b[2]))
    (a, b)             => Judgement(Symbol(a), Symbol(b), :I)
end
end

"""" BuildTypeName

Takes in an input array (AST) for a TypeName and returns a corresponding TypeName object based 
on the proper structure. Ex: Form0 versus Form0{X}.
"""
function BuildTypeName(v)
  if isempty(v[2])
    return Symbol(v[1])
  else
    return [Symbol(v[1]), Symbol(collect(Iterators.flatten(v[2]))[2])]
  end
end