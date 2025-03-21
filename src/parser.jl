import Catlab.Parsers.ParserCore

# Syntactic Analysis
####################

""" Decapode Parsing Expression Grammar

The Decapode Parsing Expression Grammar (PEG) is a formal grammar that defines the syntax of the Decapode DSL.
The grammar is used to parse Decapode code into an Abstract Syntax Tree (AST) representation which is then constructed
into an ACSet. For example, take the following string representation of a Decapode code snippet:

```julia
parse_result = decapode"
    C::Form0{X}
    (V, ϕ)::Form1{X}

    ϕ == ∧₀₁(C,V)"
```
This is parsed into the following tree structure:

```julia
# Variables
v1 = Judgement(:C, :Form0, :X)
v2  Judgement(:V, :Form1, :X)
v3 = Judgement(:ϕ, :Form1, :X)

# Equation
eq = Eq(
  DiagrammaticEquations.decapodes.Var(:ϕ),
  App2(:∧₀₁, 
    DiagrammaticEquations.decapodes.Var(:C), 
    DiagrammaticEquations.decapodes.Var(:V)
      )
  )

# Decapode Expression
Expr = DecaExpr([v1, v2, v3], [eq])
```

This AST representation is then constructed into an ACSet representation.
The mechanics of the Decapode are identical to those seen in the typical Decapode
macro. Those documents can be consulted further for information on Decapodes.
"""
# A Decapode Expression consists of a list of comments or lines of code that hold judgements and equations. 
@rule DecapodeExpr = (MultiLineComment , Line)[*] & ws |> v -> BuildExpr(v[1])

# Comments can be single line '#' or multiline '#=...=#' and are ignored by the parser.
@rule SingleLineComment = "#" & r"[^\r\n]*" |> v -> nothing
@rule MultiLineComment = "#=" & r"(?:[^=]|=(?!#)|\s)*" & "=#" |> v -> nothing

# Lines are made up of a statement or single line comments followed by an end of line character. 
@rule Line = ws & (SingleLineComment , Statement) & r"[^\S\r\n]*" & EOL |> v -> v[2]

# Statements can include either type judgements or equations.
@rule Statement = Judgement , Equation

# A judgement is a statement of the form A::B. It marks a type assignment.
@rule Judgement = (Ident , (lparen & ws & List & ws & rparen)) & "::" & TypeName |> v -> BuildJudgement(v)
# A type name can be either be of form 'name' or 'name{X}'.
@rule TypeName = Ident & ("{" & Ident & "}")[:?] |> v -> BuildTypeName(v)
 
# An equation consists of two operations matched by an "==" operator.
@rule Equation = SummationOperation & ws & "==" & ws & SummationOperation |> v -> Eq(v[1], v[5]) 

# Binary Operations are supported by the following rules.
# They also support all known Julia binary operations and their corresponding precdences.
# Lower Rules have higher precedence.
@rule SummationOperation = PrecMinusOperation & (ws & "+" & ws & PrecMinusOperation)[*] |> v -> BuildSummationOperation(v)
# Ex: -,¦,⊕,⊖,...
@rule PrecMinusOperation = PrecDivOperation & (ws & PrecMinusOp & ws & PrecDivOperation)[*] |> v -> BuildApp2(v)
# Ex: /,⌿,÷,...
@rule PrecDivOperation = MultOperation & (ws & PrecDivOp & ws & MultOperation)[*] |> v -> BuildApp2(v)
@rule MultOperation = (lparen & ws & PrecPowerOperation & ws & rparen , PrecPowerOperation) & ((ws & "*" & ws & PrecPowerOperation), PrecPowerOperation)[*] |> v -> BuildMultOperation(v)
# Ex: ^,↑,↓,...
@rule PrecPowerOperation = Term & (ws & PrecPowerOp & ws & Term)[*] |> v -> BuildApp2(v)

# Terms can consist of groupings, deriatives, function compositions, function calls, and atomic elements such as digits/identifiers.
@rule Term = Grouping , Derivative , Compose , Call , Atom

# The grouping rule supports the grouping of terms using parentheses. Highest Precdence '(...)'.
@rule Grouping = lparen & ws & SummationOperation & ws & rparen |> v -> v[3]

# The derivative rule supports derivatives of the form ∂ₜ(x) and dt(x).
@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & Ident & ws & rparen |> v -> Tan(decapodes.Var(Symbol(v[4])))

# The composition rule supports the compostion of terms A... over term b.
@rule Compose = composeCall & lparen & ws & SummationOperation & ws & rparen |> v -> AppCirc1(v[1], v[4])
# ComposeCall supports the function call name.
@rule composeCall = lparen & ws & composeList & ws & rparen |> v -> v[3],
  composeList
# ComposeList supports prefix and infix notation for function calls.
@rule composeList = "∘" & lparen & ws & CallList & ws & rparen |> v -> v[4],
  lparen & CallName & (ws & "∘" & ws & CallName)[+] & rparen |> v -> vcat(Symbol(v[2]), Symbol.(last.(v[3])))

# The call rule supports function calls of the form f(x) and g(x, y).
@rule Call = (CallName, lparen & ws & CallName & ws & rparen) & lparen & ws & Args & ws & rparen |> v -> BuildCall(v)
# A call name can be unaryoperators or identifiers
@rule CallName = UnaryOperator , Ident

# Argumennts represet a list of argument consisting of operationss within a function call
@rule Args = (SummationOperation & ws & comma & SummationOperation) |> v -> [v[1], v[4]],
  SummationOperation |> v -> [v]

# A list is a comma seperated list of identifiers.
@rule List = Ident & (ws & comma & Ident)[*] |> v -> vcat(Symbol(v[1]), Symbol.(last.(v[2])))
# A call list differs from list as it supports a comma seperated list of call names (Unary operators / Identifiers).
@rule CallList = CallName & (ws & comma & CallName)[*] |> v -> vcat(Symbol(v[1]), Symbol.(last.(v[2])))

# Atoms are the smallest unit of a term and can be either digits or identifiers.
# Identifiers can support unary operators on front Ex: Negation: '-K'.
@rule Atom = Digit , (UnaryOperator & Ident |> v -> App1(Symbol(v[1]), decapodes.Var(v[2]))), (Ident |> v -> decapodes.Var(v))

# Identifiers consists of non operator/reserved characters
# They cannot start with digits
@rule Ident = r"""([^0-9\+\*:{}→\n;=,\-−¦⊕⊖⊞⊟∪∨⊔±∓∔∸≏⊎⊻⊽⋎⋓⟇⧺⧻⨈⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨹⨺⩁⩂⩅⩊⩌⩏⩐⩒
  ⩔⩖⩗⩛⩝⩡⩢⩣\\\/⌿÷%&··⋅∘×∩∧⊗⊘⊙⊚⊛⊠⊡⊓∗∙∤⅋≀⊼⋄⋆⋇⋉⋊⋋⋌⋏⋒⟑⦸⦼⦾⦿⧶⧷⨇⨰⨱⨲⨳⨴⨵⨶⨷⨸⨻⨼⨽⩀<⩃⩄⩋⩍⩎⩑⩓⩕⩘⩚⩜
  ⩞⩟⩠⫛⊍▷⨝⟕⟖⟗⨟\^↑↓⇵⟰⟱⤈⤉⤊⤋⤒⤓⥉⥌⥍⥏⥑⥔⥕⥘⥙⥜⥝⥠⥡⥣⥥⥮⥯￪￬\|\(\)\s][^\+\*:{}→\n;=,\-−¦⊕⊖⊞⊟∪∨⊔±∓∔∸≏⊎⊻⊽⋎⋓⟇⧺⧻
  ⨈⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨹⨺⩁⩂⩅⩊⩌⩏⩐⩒⩔⩖⩗⩛⩝⩡⩢⩣\\\/⌿÷%&··⋅∘×∩∧⊗⊘⊙⊚⊛⊠⊡⊓∗∙∤⅋≀⊼⋄⋆⋇⋉⋊⋋⋌⋏⋒⟑⦸⦼⦾⦿⧶⧷⨇⨰
  ⨱⨲⨳⨴⨵⨶⨷⨸⨻⨼⨽⩀<⩃⩄⩋⩍⩎⩑⩓⩕⩘⩚⩜⩞⩟⩠⫛⊍▷⨝⟕⟖⟗⨟\^↑↓⇵⟰⟱⤈⤉⤊⤋⤒⤓⥉⥌⥍⥏⥑⥔⥕⥘⥙⥜⥝⥠⥡⥣⥥⥮⥯￪￬\|\(\)\s]*)""" |> v -> Symbol(v)

# Digits consist of numerical characters
@rule Digit = r"([\-]?)([0-9]+)(\.[0-9]+(e[0-9]+)?)?" |> v -> Lit(Symbol(v))

# Unary operators support operators that work on one operand525 170 8618
@rule UnaryOperator = PrecMinusOp , PrecDivOp , PrecPowerOp

# Unary operators with the same precedence as subtraction
@rule PrecMinusOp = r"((\.?)(\-|−|¦|⊕|⊖|⊞|⊟|∪|∨|⊔|±|∓|∔|∸|≏|⊎|⊻|⊽|⋎|⋓|⟇|⧺|⧻|⨈|⨢|⨣|⨤|⨥|⨦|⨧|⨨|⨩|
  ⨪|⨫|⨬|⨭|⨮|⨹|⨺|⩁|⩂|⩅|⩊|⩌|⩏|⩐|⩒|⩔|⩖|⩗|⩛|⩝|⩡|⩢|⩣|\|\+\+\||\|\\\|\|))|(\.\+)"x & OpSuffixes |> v -> v[1]*v[2]

# Unary operators with the same precedence as division
@rule PrecDivOp = r"((\.?)(\/|⌿|÷|%|&|·|·|⋅|×|∩|∧|⊗|⊘|⊙|⊚|⊛|⊠|⊡|⊓|∗|∙|∤|⅋|≀|⊼|⋄|⋆|⋇|⋉|⋊|⋋|⋌|⋏|⋒|⟑|
  ⦸|⦼|⦾|⦿|⧶|⧷|⨇|⨰|⨱|⨲|⨳|⨴|⨵|⨶|⨷|⨸|⨻|⨼|⨽|⩀|<|⩃|⩄|⩋|⩍|⩎|⩑|⩓|⩕|⩘|⩚|⩜|⩞|⩟|⩠|⫛|⊍|▷|⨝|⟕|⟖|⟗|⨟|\|\\\\\|))|(\.\*)"x & OpSuffixes |> v -> v[1]*v[2]

# Unary operators with the same precedence as exponential operations
@rule PrecPowerOp = r"(\.?)(\^|↑|↓|⇵|⟰|⟱|⤈|⤉|⤊|⤋|⤒|⤓|⥉|⥌|⥍|⥏|⥑|⥔|⥕|⥘|⥙|⥜|⥝|⥠|⥡|⥣|⥥|⥮|⥯|￪|￬)"  & OpSuffixes |> v -> v[1]*v[2]

# Operator suffixes allow characters that come after an operator
@rule OpSuffixes = r"[₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎²³¹ʰʲʳʷʸˡˢˣᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂᵃᵇᵈᵉᵍᵏᵐᵒᵖᵗᵘᵛᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᶜᶠᶥᶦᶫᶰᶸᶻᶿ⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿₐₑₒₓₕₖₗₘₙₚₛₜⱼⱽ′″‴‵‶‷⁗]*"

# Semantic Analysis
####################

""" BuildExpr

Takes in an input array (AST) for a DecaExpr body, filters out comments, and returns a corresponding DecaExpr object.
"""
function BuildExpr(v)
  filtered_lines = filter(x -> x !== nothing, v)
  judges = []
  eqns = []

  foreach(filtered_lines) do s
    @match s begin
      ::Judgement => push!(judges, s)
      ::Vector{Judgement} => append!(judges, s)
        ::Eq => push!(eqns, s)
        _ => error("Statement containing $s of type $(typeof(s)) was not added.")
      end
    end
    return DecaExpr(judges, eqns)
end

""" BuildJudgement

Takes in an input array (AST) for a Judgement (Type Declaration) and returns a corresponding Decapodes Judgement object
"""
function BuildJudgement(v)
  pattern = (v[1], v[3])
  @match pattern begin
    ([a...], [b...]) => map(sym -> Judgement(sym, Symbol(b[1]), Symbol(b[2])), Symbol.(a[3]))
    ([a...], b)      => map(sym -> Judgement(sym, Symbol(b), :I), Symbol.(a[3]))
    (a, [b...])      => Judgement(Symbol(a), Symbol(b[1]), Symbol(b[2]))
    (a, b)           => Judgement(Symbol(a), Symbol(b), :I)
  end
end

"""" BuildTypeName

Takes in an input array (AST) for a type name and returns a corresponding AST type name object based 
on the proper structure. Ex: Form0 versus Form0{X}.
"""
function BuildTypeName(v)
  if isempty(v[2])
    return Symbol(v[1])
  else
    return [Symbol(v[1]), Symbol(collect(Iterators.flatten(v[2]))[2])]
  end
end

""" BuildSummationOperation

Takes in an input array (AST) for a summation operation and returns a corresponding Plus object.
"""
function BuildSummationOperation(v)
  if isempty(v[2])
    return v[1]
  else
    return Plus(vcat(v[1], last.(v[2])))
  end
end

""" BuildMultOperation

Takes in an input array (AST) for a multiplication operation and returns a corresponding Mult object or App2 depending on the input size.
"""
function BuildMultOperation(v)
  Op1 = v[1]
  Op2 = v[2]

  # Check if Op1 has parenthesis or not.
  if isa(Op1, Vector) && length(Op1) == 5
    Op1 = Op1[3]
  end

  # Check if the result is an operation or term.
  if isempty(Op2)
    return Op1
  else
    normalizedOp2 = map(x -> isa(x, Vector) ? x : ["", "*", "", x], Op2)
    # Create List of operands
    multList = vcat(Op1, map(x -> [x[end]], normalizedOp2)...)
    if length(multList) == 2
      return App2(Symbol("*"), multList[1], multList[2])
    else
      return Mult(vcat(Op1, last.(normalizedOp2)))
    end
  end
end

""" BuildApp2

Takes in an input array (AST) for all binary operations except summation/multiplication and returns the result of the operation
as an App2.
"""
function BuildApp2(v)
    # Creates array of operations
    result = vcat(v[1], map(x -> [x[2], x[end]], v[2])...)
    # Converts array to tree structure
    while length(result) > 1
      Applied = App2(Symbol(result[2]), result[1], result[3])
      result = vcat([Applied], result[4:end])
    end
    return result[1]
end

""" BuildCall

Takes in an input array (AST) for a function call expression and returns a corresponding App1 or App2 object 
depending on the amount of parameters (one or two).
"""
function BuildCall(v)
  FuncCall = v[1]
  #if the function call has parentheses around it, extract the function name Ex: (f)(x) -> f(x)
  if isa(FuncCall, Vector) && length(FuncCall) == 5
    FuncCall = FuncCall[3]
  end

  if length(v[4]) == 1
      return App1(Symbol(FuncCall), v[4][1])
  else
      return App2(Symbol(FuncCall), v[4][1], v[4][2])
  end
end

# Macro Call
#############

""" Relation String Macro

This macro parses a string representation of a Decapode into an ACSet representation. It operates by parsing a string input into an DecaExpr 
(Abstract Syntax Tree) object. Then it constructs a ACSet representation from the AST.
"""
macro decapode_str(y::String)
  :(SummationDecapode(parse_whole(DecapodeExpr, $(y * "\n"))))
end