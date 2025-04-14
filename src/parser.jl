using Catlab.Parsers.ParserCore

# String -> Vector{String}
##########################

# Top-level rules
#----------------

@rule DecapodeExpr = (MultiLineComment , LineEOL)[*] & ws & Line[:?] |>
  v -> BuildExpr(vcat(v[1], v[3]))

@rule Line = ws & (SingleLineComment , Statement) & r"[^\S\r\n]*" |>
  v -> v[2]

@rule LineEOL = Line & EOL |>
  v -> v[1]

@rule Statement = Judgement , Equation

# Recall that a more-left rule in an OR has higher precedence in a PEG.
@rule Term = Derivative , Call , Compose , Grouping , Atom

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
@rule Judgement = SingleJudgement , MultiJudgement

@rule SingleJudgement = Ident & "::" & TypeName |>
  v -> Judgement(v[1], v[3]...)

@rule MultiJudgement = lparen & ws & List & ws & rparen & "::" & TypeName |>
  v -> Judgement.(v[3], v[7]...)

@rule TypeName = Ident & ("{" & Ident & "}")[:?] |>
  v -> (v[1], isempty(v[2]) ? :I : v[2][1][2])

@rule List = Ident & (ws & comma & Ident)[*] |>
  v -> vcat(v[1], last.(v[2]))

# Equations and arithmetic operations
#------------------------------------

# These rules are listed top-down: ^(*(/(+(-(==)))))

@rule Derivative = ("∂ₜ" , "dt") & lparen & ws & Ident & ws & rparen |>
  v -> Tan(Var(v[4]))

# ==
@rule Equation = PrecMinusOperation & ws & "==" & ws & PrecMinusOperation |>
  v -> Eq(v[1], v[5])

# -
@rule PrecMinusOperation = SumOperation & TrailingMinus[*] |>
  v -> BuildApp2(v[1], v[2])

@rule TrailingMinus = ws & PrecMinusOp & ws & SumOperation |>
  v -> (v[2], v[4])

# +
@rule SumOperation = NarySum , PrecDivOperation

@rule NarySum = PrecDivOperation & TrailingSum[+] |>
  v -> Plus(vcat(v[1], v[2]))

@rule TrailingSum = ws & "+" & ws & PrecDivOperation |>
  v -> v[4]

# /
@rule PrecDivOperation = MultOperation & TrailingDiv[*] |>
  v -> BuildApp2(v[1], v[2])

@rule TrailingDiv = ws & PrecDivOp & ws & MultOperation |>
  v -> (v[2], v[4])

# *
@rule MultOperation = NaryMult , BinaryMult , PrecPowerOperation

@rule BinaryMult = PrecPowerOperation & (TrailingMult , PrecPowerOperation) |>
  v -> App2(:*, v[1], v[2])

@rule NaryMult = PrecPowerOperation & (TrailingMult , PrecPowerOperation)[2:end] |>
  v -> Mult(vcat(v[1], v[2]))

@rule TrailingMult = ws & "*" & ws & PrecPowerOperation |>
  v -> v[4]

# ^
@rule PrecPowerOperation = Term & TrailingPower[*] |>
  v -> BuildApp2(v[1], v[2])

@rule TrailingPower = ws & PrecPowerOp & ws & Term |>
  v -> (v[2], v[4])

# Composition syntax
#-------------------

# e.g. ∘(⋆, d, ⋆)(C)
@rule Compose = ComposeCall & Grouping |>
  v -> AppCirc1(v[1], v[2])

@rule ComposeCall = ComposeList , ParenthesesComposeList

@rule ParenthesesComposeList = lparen & ws & ComposeList & ws & rparen |>
  v -> v[3]

@rule ComposeList = ComposePrefix , ComposeInfix

@rule ComposePrefix = "∘" & lparen & ws & CallList & ws & rparen |>
  v -> v[4]

@rule ComposeInfix = lparen & CallName & (ws & "∘" & ws & CallName)[+] & rparen |>
  v -> vcat(v[2], last.(v[3]))

@rule CallList = CallName & (ws & comma & CallName)[*] |>
  v -> vcat(v[1], last.(v[2]))

# Function syntax
#----------------

# Support function calls such as f(x) and g(x, y).
@rule Call = UnaryCall , BinaryCall

@rule UnaryCall = (CallName , ParenthesesCallName) & Grouping |>
    v -> App1(v[1], v[2])

@rule BinaryCall = (CallName , ParenthesesCallName) &
  lparen & ws & PrecMinusOperation & ws & comma & PrecMinusOperation & ws & rparen |>
    v -> App2(v[1], v[4], v[7])

@rule CallName = (Operator , Ident) |>
  v -> Symbol(v)

@rule ParenthesesCallName = lparen & ws & CallName & ws & rparen |>
  v -> v[3]

# Allowable characters
#---------------------

# Atoms are the bottom-most unit of a term.
@rule Atom = Literal , Variable , NegatedVariable

# TODO: Support underscores in literals.
@rule Literal = r"(\-)?(\d)+(\.)?(\d+)?([eE]\d+)?" |>
  v -> Lit(Symbol(v))

@rule Variable = Ident |>
  v -> Var(v)

# XXX: The negation operator, "-", can be called without parentheses.
@rule NegatedVariable = "-" & Variable |>
  v -> App1(:-, v[2])

# Identifiers consist of non operator/reserved characters.
# They cannot start with digits.
@rule Ident = r"""([^0-9\+\*:{}→\n;=,\-−¦⊕⊖⊞⊟∪∨⊔±∓∔∸≏⊎⊻⊽⋎⋓⟇⧺⧻⨈⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨹⨺⩁⩂
  ⩅⩊⩌⩏⩐⩒⩔⩖⩗⩛⩝⩡⩢⩣\\\/⌿÷%&··⋅∘×∩∧⊗⊘⊙⊚⊛⊠⊡⊓∗∙∤⅋≀⊼⋄⋆⋇⋉⋊⋋⋌⋏⋒⟑⦸⦼⦾⦿⧶⧷⨇⨰⨱⨲⨳⨴⨵⨶⨷⨸⨻⨼⨽⩀<⩃⩄⩋
  ⩍⩎⩑⩓⩕⩘⩚⩜⩞⩟⩠⫛⊍▷⨝⟕⟖⟗⨟\^↑↓⇵⟰⟱⤈⤉⤊⤋⤒⤓⥉⥌⥍⥏⥑⥔⥕⥘⥙⥜⥝⥠⥡⥣⥥⥮⥯￪￬\|\(\)\s][^\+\*:{}→\n;=,\-
  −¦⊕⊖⊞⊟∪∨⊔±∓∔∸≏⊎⊻⊽⋎⋓⟇⧺⧻⨈⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨹⨺⩁⩂⩅⩊⩌⩏⩐⩒⩔⩖⩗⩛⩝⩡⩢⩣\\\/⌿÷%&··⋅∘×∩∧⊗⊘⊙⊚⊛⊠⊡⊓
  ∗∙∤⅋≀⊼⋄⋆⋇⋉⋊⋋⋌⋏⋒⟑⦸⦼⦾⦿⧶⧷⨇⨰⨱⨲⨳⨴⨵⨶⨷⨸⨻⨼⨽⩀<⩃⩄⩋⩍⩎⩑⩓⩕⩘⩚⩜⩞⩟⩠⫛⊍▷⨝⟕⟖⟗⨟\^↑↓⇵⟰⟱⤈⤉⤊⤋⤒⤓⥉⥌⥍⥏⥑
  ⥔⥕⥘⥙⥜⥝⥠⥡⥣⥥⥮⥯￪￬\|\(\)\s]*)""" |>
  v -> Symbol(v...)

@rule Operator = PrecMinusOp , PrecDivOp , PrecPowerOp

@rule PrecMinusOp = r"((\.?)(\+|\-|−|¦|⊕|⊖|⊞|⊟|∪|∨|⊔|±|∓|∔|∸|≏|⊎|⊻|⊽|⋎|⋓|⟇|⧺|⧻|
  ⨈|⨢|⨣|⨤|⨥|⨦|⨧|⨨|⨩|⨪|⨫|⨬|⨭|⨮|⨹|⨺|⩁|⩂|⩅|⩊|⩌|⩏|⩐|⩒|⩔|⩖|⩗|⩛|⩝|⩡|⩢|⩣|\|\+\+\||
  \|\\\|\|))|(\.\+)"x & OpSuffixes |>
  v -> Symbol(v...)

@rule PrecDivOp = r"((\.?)(\*|\/|⌿|÷|%|&|·|·|⋅|×|∩|∧|⊗|⊘|⊙|⊚|⊛|⊠|⊡|⊓|∗|∙|∤|⅋|≀|
  ⊼|⋄|⋆|⋇|⋉|⋊|⋋|⋌|⋏|⋒|⟑|⦸|⦼|⦾|⦿|⧶|⧷|⨇|⨰|⨱|⨲|⨳|⨴|⨵|⨶|⨷|⨸|⨻|⨼|⨽|⩀|<|⩃|⩄|⩋|⩍|⩎|⩑|
  ⩓|⩕|⩘|⩚|⩜|⩞|⩟|⩠|⫛|⊍|▷|⨝|⟕|⟖|⟗|⨟|\|\\\\\|))|(\.\*)"x & OpSuffixes |>
  v -> Symbol(v...)

@rule PrecPowerOp = r"(\.?)(\^|↑|↓|⇵|⟰|⟱|⤈|⤉|⤊|⤋|⤒|⤓|⥉|⥌|⥍|⥏|⥑|⥔|⥕|⥘|⥙|⥜|⥝|⥠|
  ⥡|⥣|⥥|⥮|⥯|￪|￬)"x & OpSuffixes |>
  v -> Symbol(v...)

@rule OpSuffixes = r"[₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎²³¹ʰʲʳʷʸˡˢˣᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂᵃᵇᵈᵉᵍᵏᵐᵒᵖᵗᵘᵛᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᶜᶠᶥᶦᶫᶰᶸᶻᶿ⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿₐₑₒₓₕₖₗₘₙₚₛₜⱼⱽ′″‴‵‶‷⁗]*"

# Vector{String} -> ADT
#######################

# Given an array (AST), filter comments and return a DecaExpr.
function BuildExpr(v)
  judges_eqns = reduce(vcat, v, init=[])
  judges = filter(x -> x isa Judgement, judges_eqns)
  eqns   = filter(x -> x isa Eq,        judges_eqns)
  DecaExpr(judges, eqns)
end

# Given an array of binary operations, return nested App2s.
# BuildApp2(Var(:A), [["-", Var(:B)], ["-", Var(:C)]])
# returns
# App2(:-, App2(:-, Var(:A), Var(:B)), Var(:C))
function BuildApp2(pi₁, ops_pis)
  @match (pi₁, ops_pis) begin
    (Any[], _) => ops_pis
    (_, Any[]) => pi₁
    (_, [(op, pi₂), rest...]) => BuildApp2(App2(op, pi₁, pi₂), rest)
  end
end

# Macro Call
#############

""" decapode_str

This string macro converts a string representation of a Decapode into an ACSet
representation.

The Decapode Parsing Expression Grammar (PEG) is a formal grammar that defines
the syntax of the Decapode DSL. The grammar parses Decapode code into an AST.

For example, take the following string representation of a Decapode:

```julia
decapode"
  C::Form0{X}
  (V, ϕ)::Form1{X}

  ϕ == ∧₀₁(C,V)"
```
This is first parsed into the following tree structure:

```julia
js = [Judgement(:C, :Form0, :X),
      Judgement(:V, :Form1, :X),
      Judgement(:ϕ, :Form1, :X)]

eqs = [Eq(Var(:ϕ), App2(:∧₀₁, Var(:C), Var(:V)))]

Expr = DecaExpr(js, eqs)
```

The `SummationDecapode` constructor then constructs an ACSet.
"""
macro decapode_str(s::String)
  :(SummationDecapode(parse_whole(DecapodeExpr, $s)))
end

