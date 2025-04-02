using Catlab.Parsers.ParserCore

# String -> Vector{String}
##########################

# Top-level rules
#----------------

# A Decapode Expression consists of a list of comments or lines of code that
# hold judgements and equations.
@rule DecapodeExpr = (MultiLineComment , Line)[*] & ws |>
  v -> BuildExpr(v[1])

# Lines are made up of a statement or single line comments followed by an end
# of line character.
@rule Line = ws & (SingleLineComment , Statement) & r"[^\S\r\n]*" & EOL |>
  v -> v[2]

# Statements can include either type judgements or equations.
@rule Statement = Judgement , Equation

# Highest precedence:
@rule Term = Derivative , Compose , Call , Grouping , Atom

# Parentheses enforce precedence.
@rule Grouping = lparen & ws & PrecMinusOperation & ws & rparen |>
  v -> v[3]

# Comments
#---------

# Comments are ignored.
@rule SingleLineComment = "#" & r"[^\r\n]*" |>
  v -> nothing

@rule MultiLineComment = "#=" & r"(?:[^=]|=(?!#)|\s)*" & "=#" |>
  v -> nothing

# Type assignments
#------------------

# e.g. C::Form0 or (C, D)::DualForm1{X}.
@rule Judgement = (Ident , (lparen & ws & List & ws & rparen)) & "::" & TypeName |>
  v -> BuildJudgement(v[1], v[3]...)

@rule TypeName = Ident & ("{" & Ident & "}")[:?] |>
  v -> BuildTypeName(v[1], v[2])

@rule List = Ident & (ws & comma & Ident)[*] |>
  v -> vcat(Symbol(v[1]), Symbol.(last.(v[2])))

# Equations and arithmetic operations
#------------------------------------

# Recall in a PEG that a top-level rule has lower precedence.

@rule Equation = PrecMinusOperation & ws & "==" & ws & PrecMinusOperation |>
  v -> Eq(v[1], v[5]) 

@rule PrecMinusOperation = SummationOperation & (ws & PrecMinusOp & ws & SummationOperation)[*] |>
  v -> BuildApp2(v[1], v[2])

@rule SummationOperation = PrecDivOperation & (ws & "+" & ws & PrecDivOperation)[*] |>
  v -> BuildSummationOperation(v[1], v[2])

@rule PrecDivOperation = MultOperation & (ws & PrecDivOp & ws & MultOperation)[*] |>
  v -> BuildApp2(v[1], v[2])

@rule MultOperation = (PrecPowerOperation, lparen & ws & PrecPowerOperation & ws & rparen) & ((ws & "*" & ws & PrecPowerOperation), PrecPowerOperation)[*] |>
  v -> BuildMultOperation(v[1], v[2])

@rule PrecPowerOperation = Term & (ws & PrecPowerOp & ws & Term)[*] |>
  v -> BuildApp2(v[1], v[2])

@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & Ident & ws & rparen |>
  v -> Tan(Var(Symbol(v[4])))

# Composition syntax
#-------------------

# e.g. ∘(⋆, d, ⋆)(C)
@rule Compose = ComposeCall & lparen & ws & PrecMinusOperation & ws & rparen |>
  v -> AppCirc1(v[1], v[4])

@rule ComposeCall = lparen & ws & ComposeList & ws & rparen |>
    v -> v[3] ,
  ComposeList

@rule ComposeList =
  # Prefix notation
  "∘" & lparen & ws & CallList & ws & rparen |>
    v -> v[4] ,
  # Infix notation
  lparen & CallName & (ws & "∘" & ws & CallName)[+] & rparen |>
    v -> vcat(Symbol(v[2]), Symbol.(last.(v[3])))

@rule CallList = CallName & (ws & comma & CallName)[*] |>
  v -> vcat(Symbol(v[1]), Symbol.(last.(v[2])))

# Function syntax
#----------------
# The call rule supports function calls of the form f(x) and g(x, y).
@rule Call = (CallName, lparen & ws & CallName & ws & rparen) &
  lparen & ws & Args & ws & rparen |>
    v -> BuildCall(v[1], v[4])

@rule CallName = Operator , Ident

@rule Args = (PrecMinusOperation & ws & comma & PrecMinusOperation) |>
    v -> [v[1], v[4]] ,
  PrecMinusOperation |>
    v -> [v]

# Allowable characters
#---------------------

# Identifiers consist of non operator/reserved characters.
# They cannot start with digits.
@rule Ident = r"""([^0-9\+\*:{}→\n;=,\-−¦⊕⊖⊞⊟∪∨⊔±∓∔∸≏⊎⊻⊽⋎⋓⟇⧺⧻⨈⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨹⨺⩁⩂
  ⩅⩊⩌⩏⩐⩒⩔⩖⩗⩛⩝⩡⩢⩣\\\/⌿÷%&··⋅∘×∩∧⊗⊘⊙⊚⊛⊠⊡⊓∗∙∤⅋≀⊼⋄⋆⋇⋉⋊⋋⋌⋏⋒⟑⦸⦼⦾⦿⧶⧷⨇⨰⨱⨲⨳⨴⨵⨶⨷⨸⨻⨼⨽⩀<⩃⩄⩋
  ⩍⩎⩑⩓⩕⩘⩚⩜⩞⩟⩠⫛⊍▷⨝⟕⟖⟗⨟\^↑↓⇵⟰⟱⤈⤉⤊⤋⤒⤓⥉⥌⥍⥏⥑⥔⥕⥘⥙⥜⥝⥠⥡⥣⥥⥮⥯￪￬\|\(\)\s][^\+\*:{}→\n;=,\-
  −¦⊕⊖⊞⊟∪∨⊔±∓∔∸≏⊎⊻⊽⋎⋓⟇⧺⧻⨈⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨹⨺⩁⩂⩅⩊⩌⩏⩐⩒⩔⩖⩗⩛⩝⩡⩢⩣\\\/⌿÷%&··⋅∘×∩∧⊗⊘⊙⊚⊛⊠⊡⊓
  ∗∙∤⅋≀⊼⋄⋆⋇⋉⋊⋋⋌⋏⋒⟑⦸⦼⦾⦿⧶⧷⨇⨰⨱⨲⨳⨴⨵⨶⨷⨸⨻⨼⨽⩀<⩃⩄⩋⩍⩎⩑⩓⩕⩘⩚⩜⩞⩟⩠⫛⊍▷⨝⟕⟖⟗⨟\^↑↓⇵⟰⟱⤈⤉⤊⤋⤒⤓⥉⥌⥍⥏⥑
  ⥔⥕⥘⥙⥜⥝⥠⥡⥣⥥⥮⥯￪￬\|\(\)\s]*)""" |>
  v -> Symbol(v)

@rule Digit = r"([\-]?)([0-9]+)(\.[0-9]+(e[0-9]+)?)?" |>
  v -> Lit(Symbol(v))

# Atoms are the smallest unit of a term.
@rule Atom =
  Digit ,
  (Ident |>
    v -> Var(v)) ,
  # XXX: The negation operator, "-", can be called without parentheses.
  ("-" & Ident |>
    v -> App1(Symbol(v[1]), Var(v[2])))

@rule Operator = PrecMinusOp , PrecDivOp , PrecPowerOp

# Unary operators with the same precedence as subtraction
@rule PrecMinusOp = r"((\.?)(\+|\-|−|¦|⊕|⊖|⊞|⊟|∪|∨|⊔|±|∓|∔|∸|≏|⊎|⊻|⊽|⋎|⋓|⟇|⧺|⧻|
  ⨈|⨢|⨣|⨤|⨥|⨦|⨧|⨨|⨩|⨪|⨫|⨬|⨭|⨮|⨹|⨺|⩁|⩂|⩅|⩊|⩌|⩏|⩐|⩒|⩔|⩖|⩗|⩛|⩝|⩡|⩢|⩣|\|\+\+\||\|\\\|\|))|(\.\+)"x & OpSuffixes |>
  v -> string(v[1], v[2])

# Unary operators with the same precedence as division
@rule PrecDivOp = r"((\.?)(\*|\/|⌿|÷|%|&|·|·|⋅|×|∩|∧|⊗|⊘|⊙|⊚|⊛|⊠|⊡|⊓|∗|∙|∤|⅋|≀|⊼|⋄|⋆|⋇|⋉|⋊|⋋|⋌|⋏|⋒|⟑|
  ⦸|⦼|⦾|⦿|⧶|⧷|⨇|⨰|⨱|⨲|⨳|⨴|⨵|⨶|⨷|⨸|⨻|⨼|⨽|⩀|<|⩃|⩄|⩋|⩍|⩎|⩑|⩓|⩕|⩘|⩚|⩜|⩞|⩟|⩠|⫛|⊍|▷|⨝|⟕|⟖|⟗|⨟|\|\\\\\|))|(\.\*)"x & OpSuffixes |>
  v -> string(v[1], v[2])

# Unary operators with the same precedence as exponential operations
@rule PrecPowerOp = r"(\.?)(\^|↑|↓|⇵|⟰|⟱|⤈|⤉|⤊|⤋|⤒|⤓|⥉|⥌|⥍|⥏|⥑|⥔|⥕|⥘|⥙|⥜|⥝|⥠|
  ⥡|⥣|⥥|⥮|⥯|￪|￬)"x & OpSuffixes |>
  v -> string(v[1], v[2])

# Operator suffixes allow characters that come after an operator
@rule OpSuffixes = r"[₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎²³¹ʰʲʳʷʸˡˢˣᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂᵃᵇᵈᵉᵍᵏᵐᵒᵖᵗᵘᵛᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᶜᶠᶥᶦᶫᶰᶸᶻᶿ⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿₐₑₒₓₕₖₗₘₙₚₛₜⱼⱽ′″‴‵‶‷⁗]*"

# Vector{String} -> ADT
#######################

# Given an array (AST), filter comments and return a DecaExpr.
function BuildExpr(v)
  judges_eqns = reduce(vcat, v, init=[nothing])
  judges = filter(x -> x isa Judgement, judges_eqns)
  eqns   = filter(x -> x isa Eq,        judges_eqns)
  DecaExpr(judges, eqns)
end

# Given an array for a type declaration, return a Judgement.
function BuildJudgement(vars, typename, space)
  vars = vars isa AbstractVector ?
    Judgement.(Symbol.(vars[3]), typename, space) :
    Judgement(Symbol(vars), typename, space)
end

# Given an array for a type name, return a type-name: e.g. Form0 or Form0{X}.
function BuildTypeName(typename, space)
  [Symbol(typename), isempty(space) ? :I : Symbol(space[1][2])]
end

# Given an array for a summation operation, return a Plus.
function BuildSummationOperation(v1, v2)
  isempty(v2) ? v1 : Plus(vcat(v1, last.(v2)))
end

# Given an array for a multiplication operation,
# return a Mult or App2 depending on the input size.
function BuildMultOperation(pow1, pow2)
  isempty(pow2) && return pow1
  # Remove parentheses.
  pow1 = pow1 isa Vector ? pow1[3] : pow1

  # Remove matched whitespace and "*" string.
  ops = map(x -> x isa Vector ? x[4] : x, pow2)
  ops = vcat(pow1, ops)
  length(ops) == 2 ? App2(:*, ops...) : Mult(ops)
end

# Given an array of binary operations, return an App2 tree.
# Given:
# pi₁ = Lit(Symbol("3"))
# ops_pis = [[" ", "-", " ", Lit(Symbol("2"))],
#            [" ", "-", " ", Lit(Symbol("1"))]]
# Return: 
# App2(:-, App2(:-, Lit(Symbol("3")), Lit(Symbol("2"))), Lit(Symbol("1")))
function BuildApp2(pi₁, ops_pis)
  pi₁     == Any[] && return ops_pis
  ops_pis == Any[] && return pi₁
  _, op, _, pi₂ = popfirst!(ops_pis)
  BuildApp2(App2(Symbol(op), pi₁, pi₂), ops_pis)
end

# Given an array for a function call, return an App1 or App2.
# depending on the amount of parameters (one or two).
function BuildCall(func_name, args)
  # Remove parentheses. e.g. (f)(x) becomes f(x)
  # XXX: The (f)(x) syntax is not actually detected by parse_whole,
  # so this check is never called.
  func_name = func_name isa AbstractVector ? func_name[3] : func_name

  length(args) == 1 ?
    App1(Symbol(func_name), args[1]) :
    App2(Symbol(func_name), args[1], args[2])
end

# Macro Call
#############

""" decapode_str

This string macro converts a string representation of a Decapode into an ACSet
representation.

The Decapode Parsing Expression Grammar (PEG) is a formal grammar that defines
the syntax of the Decapode DSL. The grammar is used to parse Decapode code into
an AST representation.

For example, take the following string representation of a Decapode:

```julia
decapode"
  C::Form0{X}
  (V, ϕ)::Form1{X}

  ϕ == ∧₀₁(C,V)"
```
This is first parsed into the following tree structure:

```julia
# Variables
v1 = Judgement(:C, :Form0, :X)
v2 = Judgement(:V, :Form1, :X)
v3 = Judgement(:ϕ, :Form1, :X)

# Equation
eq = Eq(Var(:ϕ), App2(:∧₀₁, Var(:C), Var(:V)))

# Decapode Expression
Expr = DecaExpr([v1, v2, v3], [eq])
```

The `SummationDecapode` constructor then constructs an ACSet.
"""
macro decapode_str(y::String)
  :(SummationDecapode(parse_whole(DecapodeExpr, $(y * "\n"))))
end

