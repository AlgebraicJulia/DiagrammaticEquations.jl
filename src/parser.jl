using PEG
import Catlab.Parsers.ParserCore

export @decapode_str

# Bodies are made up of lines where each line holds a statement 
@rule DecapodeExpr = Line[*] & ws |> v -> BuildExpr(v[1])

# Lines are made up of a statement followed by an end of line character. 
@rule Line = ws & Statement & r"[^\S\r\n]*" & EOL |> v->v[2]

# Statements can include either type judgements or equations.
@rule Statement = Judgement , Equation

# A judgement is a statement of the form A::B. It marks a type assignment.
@rule Judgement = (ident , (lparen & ws & List & ws & rparen)) & "::" & TypeName |> v -> BuildJudgement(v)
@rule TypeName = ident & ("{" & ident & "}")[:?] |> v -> BuildTypeName(v)
  
@rule Equation = PlusOperation & ws & "==" & ws & PlusOperation |> v -> Eq(v[1], v[5]) 

# The operation rule supports addition and multiplication of terms.
@rule PlusOperation = MultOperation & (ws & "+" & ws & MultOperation)[*] |> v -> BuildPlusOperation(v)
@rule MultOperation = Term & (ws & "*" & ws & Term)[*] |> v -> BuildMultOperation(v)

@rule Term = Grouping, Derivative, Compose, Call, ident |> v -> ParseIdent(v)

# The grouping rule supports the grouping of terms using parentheses. Higher precedence than +/*.
@rule Grouping = lparen & ws & PlusOperation & ws & rparen |> v -> v[3]

# The derivative rule supports derivatives of the form ∂ₜ(x) and dt(x).
@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & ident & ws & rparen |> v -> Tan(decapodes.Var(Symbol(v[4])))

# The composition rule supports the compostion of terms A over term b.
@rule Compose = "∘" & lparen & ws & List & rparen & ws & lparen & ws & MultOperation & rparen |> v -> AppCirc1(v[4], v[9])

# The call rule supports function calls of the form f(x) and g(x, y).
@rule Call = ident & lparen & ws & Args & ws & rparen |> v -> BuildCall(v)
@rule Args = (PlusOperation & ws & "," & ws & PlusOperation) |> v -> [v[1], v[5]],
PlusOperation |> v -> [v]

@rule List = ident & (ws & comma & ws & ident)[*] |> v -> vcat(Symbol(v[1]), Symbol.(last.(v[2])))

@rule ident = r"[^\+*:{}→\n;=,\(\)\s]+" # Catlab ident does not support removal of `+` and `*` characters.

""" BuildMultOperation

Takes in an input array (AST) for a multiplication operation and returns a corresponding Mult object. Handles non mult operations as well.
"""
function BuildMultOperation(v)
  if isempty(v[2])
    return v[1]
  else
    return Mult(vcat(v[1], last.(v[2])))
  end
end

""" BuildPlusOperation

Takes in an input array (AST) for a multiplication operation and returns a corresponding Mult object. Handles non mult operations as well.
"""
function BuildPlusOperation(v)
  if isempty(v[2])
    return v[1]
  else
    return Plus(vcat(v[1], last.(v[2])))
  end
end

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

""" BuildJudgement

Takes in an input array (AST) for a Judgement corresponding Judgement object
"""
function BuildJudgement(v)
  pattern = (v[1], v[3])
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

"""
BuildExpr

Takes in an input array (AST) for a body and returns a corresponding DecaExpr object.
"""
function BuildExpr(v)
  judges = []
  eqns = []
  foreach(v) do s
    @match s begin
      ::Judgement => push!(judges, s)
      ::Vector{Judgement} => append!(judges, s)
        ::Eq => push!(eqns, s)
        _ => error("Statement containing $s of type $(typeof(s)) was not added.")
      end
    end
    return DecaExpr(judges, eqns)
end

""" Relation String Macro

This macro parses a string representation of a UWD into an ACSet representation. It operates by parsing a string input into an UWDExpr object.
Then it constructs a RelationDiagram object from the UWDExpr object.
"""
macro decapode_str(y::String)
  :(SummationDecapode(parse_whole(DecapodeExpr, $(y * "\n"))))
end