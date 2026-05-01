using Test
import Catlab.CategoricalAlgebra: is_isomorphic

using DiagrammaticEquations

# Import Decapodes AST Nodes
using DiagrammaticEquations: Lit, AppCirc1, App1, App2, Plus, Mult, Tan, Eq, Var, Judgement

# Import PEG Rules
using PEG
import DiagrammaticEquations.Parser: DecapodeExpr, SingleLineComment, MultiLineComment,
  Line, Statement, AnyJudgement, TypeName, Equation, SumOperation,
  PrecMinusOperation, PrecDivOperation, MultOperation, PrecPowerOperation, Term as ParserTerm,
  Grouping, Derivative, Compose, Call, CallName, Operator, List, CallList, Atom,
  Ident, SciLiteral, PrecMinusOp, PrecDivOp, PrecPowerOp, OpSuffixes, LieBracket, Magnitude,
  @decapode_str

LS = Lit ∘ Symbol

# Unit Tests
#############

@testset "Overall Decapode Expression" begin
  @test DecapodeExpr("a::b\nc == d\ndt(X) == Y\n")[1] ==
    DecaExpr([Judgement(:a, :b, :I)], [Eq(Var(:c), Var(:d)), Eq(Tan(Var(:X)), Var(:Y))])
  @test DecapodeExpr(" \n")[1] == DecaExpr([],[])
  @test DecapodeExpr("a::b\n")[1] == DecaExpr([Judgement(:a, :b, :I)],[])
  @test DecapodeExpr("a::b\nx::y\nz::l\na == b\n")[1] == DecaExpr(
    [Judgement(:a, :b, :I), Judgement(:x, :y, :I), Judgement(:z, :l, :I)],
    [Eq(Var(:a), Var(:b))])
end

@testset "Comments" begin
  @test DecapodeExpr("a::b\n# This is a comment\n")[1] ==
    DecaExpr([Judgement(:a, :b, :I)], [])
  @test DecapodeExpr("a::b\n#a::b\n")[1] ==
    DecaExpr([Judgement(:a, :b, :I)], [])
  @test DecapodeExpr("# This is a comment\na::b\n")[1] ==
    DecaExpr([Judgement(:a, :b, :I)], [])
  @test DecapodeExpr("a::b\n# This is comment\nc == d\n")[1] ==
    DecaExpr([Judgement(:a, :b, :I)], [Eq(Var(:c), Var(:d))])
  @test DecapodeExpr("#= This is a multi-line comment\nspanning multiple lines\n=# a::b\n")[1] ==
    DecaExpr([Judgement(:a, :b, :I)], [])
  @test DecapodeExpr("a::b\n#= Multi-line comment\nspanning lines\n=#\nc == d\n")[1] ==
    DecaExpr([Judgement(:a, :b, :I)], [Eq(Var(:c), Var(:d))])
  @test DecapodeExpr("#=\nMulti-line comment\nspanning lines\n=#\nA == d(B)\n")[1] ==
    DecaExpr([], [Eq(Var(:A), App1(:d, Var(:B)))])
  @test DecapodeExpr("A == d(B)\n#=\nMulti-line comment\nspanning lines\n=#\n")[1] ==
    DecaExpr([], [Eq(Var(:A), App1(:d, Var(:B)))])
end

@testset "Lines" begin
  @test Line("a::b\n")[1] == Judgement(:a, :b, :I)
  @test Line("alpha::beta\n")[1] == Judgement(:alpha, :beta, :I)
  @test Line("x::y\n")[1] == Judgement(:x, :y, :I)
  @test Line("a == b\n")[1] == Eq(Var(:a), Var(:b))
  @test Line("Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)\n")[1] ==
    Eq(Var(Symbol("Ċ")), AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁], Var(:ϕ)))
end

@testset "Statements" begin
  @test DiagrammaticEquations.Parser.Statement("a::b")[1] == Judgement(:a, :b, :I)
  @test DiagrammaticEquations.Parser.Statement("a == b")[1] == Eq(Var(:a), Var(:b))
end

@testset "Judgements" begin
  @test AnyJudgement("alpha::beta")[1] == Judgement(:alpha, :beta, :I)
  @test AnyJudgement("a::Form{X}")[1] == Judgement(:a, :Form, :X)
  @test AnyJudgement("(a, b, c)::d")[1] ==
    [Judgement(:a, :d, :I), Judgement(:b, :d, :I), Judgement(:c, :d, :I)]
  @test AnyJudgement("(a, b, c)::Form{X}")[1] ==
    [Judgement(:a, :Form, :X), Judgement(:b, :Form, :X), Judgement(:c, :Form, :X)]
end

@testset "Type Naming" begin
  @test TypeName("Form0")[1] == (:Form0, :I)
  @test TypeName("Form0{X}")[1] == (:Form0, :X)
end

@testset "Equation" begin
  @test Equation("a == b")[1] == Eq(Var(:a), Var(:b))
  @test Equation("a 
  ==
   b")[1] ==
    Eq(Var(:a), Var(:b))
  @test Equation("dt( X ) == Y")[1] == Eq(Tan(Var(:X)), Var(:Y))
  @test Equation("f(n+1, N+2) * ∂ₜ(X) == ∘(a, b)(c)")[1] == Eq(
    App2(:*, App2(:f,
      Plus([Var(:n), LS("1")]),
      Plus([Var(:N), LS("2")])),
    Tan(Var(:X))),
    AppCirc1([:a, :b], Var(:c)))
  @test Equation("Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)")[1] == Eq(
    Var(Symbol("Ċ")),
    AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁], Var(:ϕ)))
end

@testset "Summation Operation" begin
  @test SumOperation("a + b")[1] ==
    Plus([Var(:a), Var(:b)])
  @test SumOperation("a + b + c")[1] ==
    Plus([Var(:a), Var(:b), Var(:c)])
  @test SumOperation("dt(X) + ∂ₜ(X)")[1] ==
    Plus([Tan(Var(:X)), Tan(Var(:X))])
  @test SumOperation("a * b + c")[1] ==
    Plus([App2(:*, Var(:a), Var(:b)), Var(:c)])
  @test SumOperation("3 * 5 + 2")[1] ==
    Plus([App2(:*, LS("3"), LS("5")),LS("2")])
  @test SumOperation("10 / 2 + 3")[1] ==
    Plus([App2(:/, LS("10"), LS("2")), LS("3")]) 
  @test SumOperation("3 * (5 + 2)")[1] ==
    App2(:*, LS("3"), Plus([LS("5"), LS("2")]))
end

@testset "Subtraction Precedence Operations" begin
  @test PrecMinusOperation("3 - 2")[1] ==
    App2(:-, LS("3"), LS("2"))
  @test PrecMinusOperation("3 - 2 - 1")[1] ==
    App2(:-, App2(:-, LS("3"), LS("2")), LS("1"))
  @test PrecMinusOperation("3 .- 2")[1] ==
    App2(:.-, LS("3"), LS("2"))
  @test PrecMinusOperation("3 ⊕₀₁ 4")[1] ==
    App2(:⊕₀₁, LS("3"), LS("4"))
  @test PrecMinusOperation("-3 - 4")[1] ==
    App2(:-, LS("-3"), LS("4"))
  @test PrecMinusOperation("3 - -4")[1] ==
    App2(:-, LS("3"), LS("-4"))
end

@testset "Division Precedence Operations" begin
  @test PrecDivOperation("10/2")[1] == App2(:/, LS("10"), LS("2"))
  @test PrecDivOperation("10 / 2")[1] == App2(:/, LS("10"), LS("2"))
  @test PrecDivOperation("10 ∧ 2")[1] == App2(:∧, LS("10"), LS("2"))
  @test PrecDivOperation("C ∧₀₁ V")[1] == App2(:∧₀₁, Var(:C), Var(:V))
  @test PrecDivOperation("A .* B")[1] == App2(:.*, Var(:A), Var(:B))
end

@testset "Multiplication Operations" begin
  @test MultOperation("a * b")[1] == App2(:*, Var(:a), Var(:b))
  @test MultOperation("(a)b")[1] == App2(:*, Var(:a), Var(:b))
  @test MultOperation("3(a)b")[1] ==
    Mult([LS("3"), Var(:a), Var(:b)])
  @test MultOperation("(a) * b")[1] == App2(:*, Var(:a), Var(:b))
  @test MultOperation("a * 
  b")[1] ==
    App2(:*, Var(:a), Var(:b))
  @test MultOperation("a * b * c")[1] ==
    Mult([Var(:a), Var(:b), Var(:c)])
  @test MultOperation("2*d₀(C)")[1] ==
    App2(:*, LS("2"), App1(:d₀, Var(:C)))
  @test MultOperation("f(n+1, N+2) * ∂ₜ(X)")[1] ==
    App2(:*, 
      App2(:f,
        Plus([Var(:n), LS("1")]),
        Plus([Var(:N), LS("2")])),
      Tan(Var(:X)))
end

@testset "Power Precedence Operations" begin
  @test PrecPowerOperation("a^b")[1] == App2(:^, Var(:a), Var(:b))
  @test PrecPowerOperation("0.5^b")[1] == App2(:^, LS("0.5"), Var(:b))
end

@testset "Terms" begin
  @test ParserTerm("∂ₜ(X)")[1] == Tan(Var(:X))
  @test ParserTerm("a")[1] == Var(:a)
  @test ParserTerm("12")[1] == LS("12")
  @test ParserTerm("∘(a, b)(c)")[1] == AppCirc1([:a, :b], Var(:c))
  @test ParserTerm("a(b)")[1] == App1(:a, Var(:b))
end

@testset "Grouping" begin
  @test Grouping("(a)")[1] == Var(:a)
  @test Grouping("(a + b)")[1] == Plus([Var(:a), Var(:b)])
end

@testset "Derivatives" begin
   @test Derivative("dt( X )")[1] == Tan(Var(:X))
   @test Derivative("∂ₜ(X)")[1] == Tan(Var(:X))
end

@testset "Compose" begin
  @test Compose("∘(a, b)(c)")[1] == AppCirc1([:a, :b], Var(:c))
  @test Compose("(∘(a, b))(c)")[1] == AppCirc1([:a, :b], Var(:c))
  @test Compose("∘(a, b, c)(d)")[1] == AppCirc1([:a, :b, :c], Var(:d))
  @test Compose("∘(a)(∂ₜ(X))")[1] == AppCirc1([:a], Tan(Var(:X)))
  @test Compose("∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)")[1] ==
    AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁], Var(:ϕ))
  @test Compose("(⋆ ∘ ⋆)(C ∧ dX)")[1] ==
    AppCirc1([:⋆, :⋆], App2(:∧, Var(:C), Var(:dX)))
  @test Compose("((⋆ ∘ ⋆))(C ∧ dX)")[1] ==
    AppCirc1([:⋆, :⋆], App2(:∧, Var(:C), Var(:dX)))
end

@testset "Function Call" begin
  @test Call("a(b)")[1] == App1(:a, Var(:b))
  @test Call("a(b, c)")[1] == App2(:a, Var(:b), Var(:c))
  @test Call("f(n + 1)")[1] ==
    App1(:f, Plus([Var(:n), LS("1")]))
  @test Call("f(n+1, N+2)")[1] ==
    App2(:f,
      Plus([Var(:n), LS("1")]),
      Plus([Var(:N), LS("2")]))
  @test Call("⊕(a, b)")[1] == App2(:⊕, Var(:a), Var(:b))
  @test Call("⊕(a)")[1] == App1(:⊕, Var(:a))
  @test Call("HI(a, b)")[1] == App2(:HI, Var(:a), Var(:b))
  @test Call("d(Ψ)")[1] == App1(:d, Var(:Ψ))
  @test Call("(d)(Ψ)")[1] == App1(:d, Var(:Ψ))
end

@testset "Prefix Notation" begin
  @test Call("+(3, 3)")[1] == App2(:+, LS("3"), LS("3"))
  @test Call("*(3, 2)")[1] == App2(:*, LS("3"), LS("2"))
  @test Call("∧(A, 2)")[1] == App2(:∧, Var(:A), LS("2"))
end

@testset "Circumfix Notation" begin
  @test LieBracket("[C,D]")[1] == App2(:L₁, Var(:C), Var(:D))
  @test Magnitude("|x|")[1] == App1(:mag, Var(:x))
  @test Magnitude("|f(x)|")[1] == App1(:mag, App1(:f, Var(:x)))
end

@testset "Function Call Names" begin
  # Identifier Case
  @test CallName("f")[1] == :f
  # Unary Case
  @test CallName("⊕")[1] == :⊕
end

@testset "Operators" begin
  @test Operator("-")[1] == :-
  @test Operator("⊕")[1] == :⊕
  @test Operator("⊽")[1] == :⊽
end

@testset "List" begin
  @test List("a, b")[1] == [:a, :b]
  @test List("a, b, c")[1] == [:a, :b, :c]
end

@testset "Call List" begin
  @test CallList("a, b")[1] == [:a, :b]
  @test CallList("⊕, ⊽, A")[1] == [:⊕, :⊽, :A]
end

@testset "Atoms" begin
  @test Atom("a")[1] == Var(:a)
  @test Atom("23")[1] == LS("23")
  @test Atom("-2")[1] == LS("-2")
  @test Atom("32.23")[1] == LS("32.23")
  @test Atom("1.65e7")[1] == LS("1.65e7")
end

@testset "Identifiers" begin
  @test Ident("abc")[1] == :abc
  @test Ident("Ċ")[1] == Symbol("Ċ")
  @test Ident("abbc")[1] == :abbc
  @test Ident("a")[1] == :a
end

@testset "Literals" begin
  @test SciLiteral("1")[1] == LS("1")
  @test SciLiteral("-2")[1] == LS("-2")
  @test SciLiteral("1231232")[1] == LS("1231232")
  @test SciLiteral("1e1")[1] == LS("1e1")
  @test SciLiteral("1E1")[1] == LS("1E1")
  @test SciLiteral("1.e1")[1] == LS("1.e1")
  @test SciLiteral("1.E1")[1] == LS("1.E1")
  @test SciLiteral("1.")[1] == LS("1.")
  @test SciLiteral("1_200.e1")[1] == LS("1_200.e1")
  @test SciLiteral("1_200.E1_300")[1] == LS("1_200.E1_300")
end

@testset "Subtraction Precedence Operator" begin
  @test PrecMinusOp("-")[1] == :-
  @test PrecMinusOp("⊕")[1] == :⊕
  @test PrecMinusOp("⊕²³")[1] == :⊕²³
  @test PrecMinusOp("⨨")[1] == :⨨
  @test PrecMinusOp("⨪")[1] == :⨪
end

@testset "Division Precedence Operator" begin
  @test PrecDivOp("/")[1] == :/
  @test PrecDivOp("∧")[1] == :∧
  @test PrecDivOp(".*")[1] == :.*
  @test PrecDivOp("×₆₇")[1] == :×₆₇
  @test PrecDivOp("⋉")[1] == :⋉
  @test PrecDivOp("⦸")[1] == :⦸
end

@testset "Power Precedence Operator" begin
  @test PrecPowerOp("^")[1] == :^
  @test PrecPowerOp("↑")[1] == :↑
  @test PrecPowerOp("⤋ʷ")[1] == :⤋ʷ
end

@testset "Operator Suffixes" begin
  @test OpSuffixes("₁₂₃₄₅")[1] == "₁₂₃₄₅" 
end

@testset "Calls Versus Multiplication" begin
  @test MultOperation("(d)(ψ)")[1] == App1(:d, Var(:ψ))
  @test MultOperation("d(ψ)")[1] == App1(:d, Var(:ψ))
  @test MultOperation("(d)ψ")[1] == App2(:*, Var(:d), Var(:ψ))
  @test MultOperation("(d)(a, b)")[1] == App2(:d, Var(:a), Var(:b))
end

# Exception Handling Tests
##########################

# Taken from "PEG.jl/blob/master/test/misc.jl": Returns byte where parsing fails.
function parse_fails_at(rule, input)
  try
    parse_whole(rule, input)
    "parse succeeded!"
  catch err
    isa(err, Meta.ParseError) || rethrow()
    m = match(r"^On line \d+, at column \d+ \(byte (\d+)\):", err.msg)
    m == nothing && rethrow()
    parse(Int, m.captures[1])
  end
end

@testset "Exception Handling" begin
  @test parse_fails_at(List, "abcd efgh ijkl") == 6
  @test parse_fails_at(List, "abcd efgh, ijkl") == 6

  @test parse_fails_at(Call, "ident(ident") == 12
  @test parse_fails_at(Call, "ident ident)") == 6
  @test parse_fails_at(Call, "ident()") == 7

  @test parse_fails_at(Compose, "∘(ident)") == 11
  @test parse_fails_at(Compose, "∘(ident)ident") == 11
  @test parse_fails_at(Compose, "∘ident(ident)") == 4
  
  @test parse_fails_at(Derivative, "ident(X)") == 1
  @test parse_fails_at(Derivative, "∂ₜident") == 7
  @test parse_fails_at(Derivative, "∂ₜ(X") == 9

  @test parse_fails_at(Grouping, "(ident") == 7

  @test parse_fails_at(MultOperation, "ident *") == 8
  @test parse_fails_at(MultOperation, "2 3") == 3

  @test parse_fails_at(SumOperation, " + 3") == 1

  @test parse_fails_at(Statement, "A B") == 3
  @test parse_fails_at(Statement, "A == ") == 6
  @test parse_fails_at(Statement, "A == B == C") == 8

  @test parse_fails_at(TypeName, "Form0{}") == 7
  # Interestingly fails at the identifier before the }.
  # Seems to be a handling difference with the PEG. 
  @test parse_fails_at(TypeName, "Form0{X ") == 7

  @test parse_fails_at(Statement, "ident::") == 8
  @test parse_fails_at(Statement, "::ident") == 1
end

# END-END Tests
##################

function parsing_is_isomorphic(code)
  macro_entry = parse_decapode(eval(Meta.parse("quote\n$(code)\nend")))
  str_entry = parse_whole(DecapodeExpr, code * "\n")

  @test is_isomorphic(SummationDecapode(macro_entry), SummationDecapode(str_entry))
end

@testset "End to End" begin
  # Vars can only be of certain types.
  @test_throws ErrorException decapode"
    (C)::Foo
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    Ċ == ∂ₜ(C)"

  # decapode"" parses an inplace ∘ syntax without parentheses.
  # This diverges from the behavior of the @decapode macro.
  @test is_isomorphic(
    decapode"
      C::Form0
      V::Form1
      dX::Form1
                
      V == (⋆ ∘ ⋆)(C ∧ dX)",
    @decapode begin
      C::Form0
      V::Form1
      dX::Form1
                
      V == ((⋆) ∘ (⋆))(C ∧ dX)
    end)

  # Ensure the decapode"" string literal is explicitly called in this test file.
  @test is_isomorphic(
    decapode"
      h::Form0
      Γ::Form1
      n::Constant
                
      ∂ₜ(h) == (∘(⋆, d, ⋆))(((Γ * d(h)) ∧ mag(♯(d(h))) ^ (n - 1)) ∧ h ^ (n + 2))",
    @decapode begin
      h::Form0
      Γ::Form1
      n::Constant
                
      ∂ₜ(h) == (∘(⋆, d, ⋆))(((Γ * d(h)) ∧ mag(♯(d(h))) ^ (n - 1)) ∧ h ^ (n + 2))
    end)

  # Support the magnitude circumfix notation |expr| in the Halfar equation.
  @test is_isomorphic(
    decapode"
      h::Form0
      Γ::Form1
      n::Constant

      ∂ₜ(h) == (∘(⋆, d, ⋆))(((Γ * d(h)) ∧ |♯(d(h))| ^ (n - 1)) ∧ h ^ (n + 2))",
    @decapode begin
      h::Form0
      Γ::Form1
      n::Constant

      ∂ₜ(h) == (∘(⋆, d, ⋆))(((Γ * d(h)) ∧ mag(♯(d(h))) ^ (n - 1)) ∧ h ^ (n + 2))
    end)
  
  # Support parsing literals.
  parsing_is_isomorphic("
    (C, Ċ)::Form0
    ϕ::Form1
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ")
  
  # Multiply without explicitly giving parentheses.
  parsing_is_isomorphic("
    (C, Ċ)::Form0
    ϕ::Form1
    ϕ ==  4*2*3*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ")

  # Variables need not be declared before use.
  parsing_is_isomorphic("
    Ċ::Form0
    ϕ ==  4*2*3*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ")

  parsing_is_isomorphic("
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ")

  # TVars can be parsed on either side of an equation.
  parsing_is_isomorphic("
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    Ċ == ∂ₜ(C)")

  parsing_is_isomorphic("
    ϕ ==  2*d₀(C)
    ∂ₜ(C) == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)")

  # Note that the first decapode types X as an op1,
  # while the second is a multiplication between X and F
  parsing_is_isomorphic("
    (A, B, X)::Form0{X}
    A == X(F)")
  parsing_is_isomorphic("
    (A, B, X)::Form0{X}
    A == (X)F")

  # Mixed Semicolon support test
  parsing_is_isomorphic("
    (C, Ċ)::Form0{X}; (ϕ, ϕ₁, ϕ₂)::Form1{X};

    ϕ == ϕ₁ + ϕ₂; Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ);
    ∂ₜ(C) == Ċ")

  # Ensure (f)(x) syntax is parsed as a unary operation.
  # i.e. The `Call` rule is matched before the multiplication rule.
  parsing_is_isomorphic("
    A == (d)(ψ)")

  # Note that literals are not evaluated before being parsed.
  # i.e. "2E9" would not be expanded to :2.0e9 until `parse` is finally
  # called during `gensim`.
  parsing_is_isomorphic("
    Γ::Form1
    (A, ρ, g, n)::Constant

    Γ == (2.0e9 / (n + 2)) * A * (ρ * g) ^ n")

  # Generic Decapodes
  #------------------

  parsing_is_isomorphic("
    h::Form0
    Γ::Form1
    n::Constant
              
    ∂ₜ(h) == (∘(⋆, d, ⋆))(((Γ * d(h)) ∧ mag(♯(d(h))) ^ (n - 1)) ∧ h ^ (n + 2))")

  parsing_is_isomorphic("
    (C, Ċ)::Form0
    (ϕ, ϕ₁, ϕ₂)::Form1

    ϕ == ϕ₁ + ϕ₂
    Ċ == (⋆₀⁻¹)(dual_d₁(⋆₁(ϕ)))
    ∂ₜ(C) == Ċ")
  
  parsing_is_isomorphic("
    (C, Ċ)::Form0
    ϕ::Form1
    ϕ ==  ∘(k, d₀)(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ")

  parsing_is_isomorphic("
    D == ∂ₜ(C)
    E == ∂ₜ(D)")

  parsing_is_isomorphic("
    X::Form0{Point}
    V::Form0{Point}

    k::Constant{Point}

    ∂ₜ(X) == V
    ∂ₜ(V) == -1*k*(X)")

  parsing_is_isomorphic("
    x::Form0{X}
    y::Form0{X}
    z::Form0{X}

    ∂ₜ(z) == f1(x) + ∘(g, h)(y)
    y == F(f2(x), ρ(x,z))")

  parsing_is_isomorphic("
    (C, Ċ)::Form0{X}
    ϕ::Form1{X}

    # Fick's first law
    ϕ ==  ∘(k, d₀)(C)
    # Diffusion equation
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ")

  parsing_is_isomorphic("
    C::Form0{X}
    (V, ϕ)::Form1{X}

    ϕ == ∧₀₁(C,V)")

  parsing_is_isomorphic("
    (C, Ċ)::Form0{X}
    (ϕ, ϕ₁, ϕ₂)::Form1{X}

    ϕ == ϕ₁ + ϕ₂
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ")

  parsing_is_isomorphic("
    (HT, Tₛ)::Form0
    (D, cosϕᵖ, cosϕᵈ)::Constant
    
    HT == (D ./ cosϕᵖ) .* (⋆)(d(cosϕᵈ .* (⋆)(d(Tₛ))))")

  parsing_is_isomorphic("
    (Tₛ, OLR)::Form0
    (A, B)::Constant
    
    OLR == A .+ B .* Tₛ")

  parsing_is_isomorphic("
    (Q, ASR)::Form0
    α::Constant
    
    ASR == (1 .- α) .* Q")

  parsing_is_isomorphic("
    C::Form0
    ϕ::Form1
              
    ϕ == k(d₀(C))")

  parsing_is_isomorphic("
    h::Form0
    (𝐮, w)::DualForm1
              
    w == (1 - σ(h)) ∧ᵖᵈ₀₁ 𝐮")

  parsing_is_isomorphic("
    (ρ, Ψ)::Form0
    β⁻¹::Constant

    ∂ₜ(ρ) == (∘(⋆, d, ⋆))(d(Ψ) ∧ ρ) + β⁻¹ * Δ(ρ)")

  parsing_is_isomorphic("
    (𝐮, w)::DualForm1
    (P, 𝑝ᵈ)::DualForm0
    μ::Constant

    𝑝ᵈ == P + 0.5 * ι₁₁(w, w)

    ∂ₜ(𝐮) == μ * (∘(d, ⋆, d, ⋆))(w) + -1 * (⋆₁⁻¹)(w ∧ᵈᵖ₁₀ (⋆)(d(w))) + d(𝑝ᵈ)")

  parsing_is_isomorphic("
    (f, b)::Form0
    (v, V, g, Fᵥ, uˢ, v_up)::Form1
    τ::Form2
    U::Parameter

    uˢ̇ == ∂ₜ(uˢ)

    v_up == (((((((-1 * L(v, v) - L(V, v)) - L(v, V)) - f ∧ v) -
      (∘(⋆, d, ⋆))(uˢ) ∧ v) - d(p)) + b ∧ g) - (∘(⋆, d, ⋆))(τ)) + uˢ̇ + Fᵥ

    uˢ̇ == force(U)")

  parsing_is_isomorphic("
    (V, V̇, G)::Form1{X}
    (ρ, ṗ, p)::Form0{X}

    V̇ == neg₁(L₁′(V, V)) +
      div₁(kᵥ(Δ₁(V) + third(d₀(δ₁(V)))), avg₀₁(ρ)) +
      d₀(half(i₁′(V, V))) +
      neg₁(div₁(d₀(p), avg₀₁(ρ))) +
      G
    ∂ₜ(V) == V̇

    ṗ == neg₀((⋆₀⁻¹)(L₀(V, (⋆₀)(p))))
    ∂ₜ(p) == ṗ")

  parsing_is_isomorphic("
    X::Form0
    V::Form0
    k::Constant
    
    ∂ₜ(X) == V
    ∂ₜ(V) == -k * X")

  parsing_is_isomorphic("
    P::Form0
    q::Form1
    (R, μ̃)::Constant

    Δq == Δ(q)
    ∇P == d(P)

    ∂ₜ(q) == q̇
    
    q̇ == μ̃ * ∂q(Δq) + ∇P + R * q")

  parsing_is_isomorphic("
    q::Form1
    (P, ρ)::Form0
    (k, R, μ̃)::Constant

    ∂ₜ(q) == q̇
    ∇P == d(P)

    q̇ == (μ̃ * ∂q(Δ(q)) - ∇P) + R * q
    P == k * ρ

    ∂ₜ(ρ) == ρ̇

    ρ_up == (∘(⋆, d, ⋆))(-1 * (ρ ∧₀₁ q))

    ρ̇ == ∂ρ(ρ_up)")
  
  parsing_is_isomorphic("
    (i, h, m)::Constant
    V::Parameter
    Ψ::Form0

    ∂ₜ(Ψ) == (((-1 * h ^ 2) / (2m)) * Δ(Ψ) + V * Ψ) / (i * h)")

  parsing_is_isomorphic("
    (U, V)::Form0
    UV2::Form0
    (U̇, V̇)::Form0
    (f, k, rᵤ, rᵥ)::Constant

    UV2 == U .* (V .* V)
    U̇ == (rᵤ * Δ(U) - UV2) + f * (1 .- U)
    V̇ == (rᵥ * Δ(V) + UV2) - (f + k) .* V

    ∂ₜ(U) == U̇
    ∂ₜ(V) == V̇")

  parsing_is_isomorphic("
    (U, V)::Form0
    U2V::Form0
    (U̇, V̇)::Form0
    α::Constant
    F::Parameter

    U2V == (U .* U) .* V
    U̇ == ((1 + U2V) - 4.4U) + α * Δ(U) + F
    V̇ == (3.4U - U2V) + α * Δ(V)

    ∂ₜ(U) == U̇
    ∂ₜ(V) == V̇")

  parsing_is_isomorphic("
    (n, w)::DualForm0
    dX::Form1
    (a, ν)::Constant

    ∂ₜ(w) == ((a - w) - w * n ^ 2) + ν * Δ(w)")

  parsing_is_isomorphic("
    (n, w)::DualForm0
    dX::Form1
    (a, ν)::Constant

    ∂ₜ(w) == ((a - w) - w * n ^ 2) + ν * ℒ(dX, w)")

  parsing_is_isomorphic("
    (n, w)::DualForm0
    m::Constant

    ∂ₜ(n) == (w * n ^ 2 - m * n) + Δ(n)")

  parsing_is_isomorphic("
    ρ::Form0
    (μ, Λ, L)::Constant

    ∂ₜ(ρ) == (ρ * (((1 - μ) + (Λ - 1) * ρ) - ρ * ρ) + 0.5 * (L * L - ρ) * Δ(ρ)) -
      0.125 * ρ * Δ(ρ) * Δ(ρ)")

  parsing_is_isomorphic("
    (X, Y)::Form0
    (μ, ν, a, b, c, d)::Constant

    ∂ₜ(X) == a * X + b * Y + μ * Δ(X)
    ∂ₜ(Y) == c * X + d * Y + ν * Δ(X)")

  parsing_is_isomorphic("
    (S, T)::Form0
    (Ṡ, T_up)::Form0
    v::Form1
    v_up::Form1

    Ṫ == ∂ₜ(T)
    Ṡ == ∂ₜ(S)
    v̇ == ∂ₜ(v)
    
    Ṫ == ∂_spatial(T_up)
    v̇ == ∂_noslip(v_up)")

  parsing_is_isomorphic("
    (Tₛ, ASR, OLR, HT)::Form0
    C::Constant

    Tₛ̇ == ∂ₜ(Tₛ)

    Tₛ̇ == ((ASR - OLR) + HT) ./ C")

  parsing_is_isomorphic("
    (b, T, S)::Form0
    (g, α, β)::Constant

    b == g * (α * T - β * S)")

  parsing_is_isomorphic("
    Q::Form0
    cosϕᵖ::Constant

    Q == 450cosϕᵖ")

  parsing_is_isomorphic("
    (c, C, F, c_up)::Form0
    (v, V, q)::Form1

    c_up == (((-1 * (⋆)(L(v, (⋆)(c))) - (⋆)(L(V, (⋆)(c)))) - (⋆)(L(v, (⋆)(C))))
      - (∘(⋆, d, ⋆))(q)) + F")

  parsing_is_isomorphic("
    Tₛ::Form0
    A::Form1

    A == avg₀₁(5.8282 * 10 ^ (-0.236Tₛ) * 1.65e7)")

  # Multiline Test
  parsing_is_isomorphic("
    #=
      Multi-line comment
    =#
    A == d(B)
    ")

  parsing_is_isomorphic("
    A == d(B)
    #=
      Multi-line comment
    =#
    ")
  parsing_is_isomorphic("
    A::Form0
    #=
      Multi-line comment
    =#
    A == d(B)
    ")

  # Support parsing the Lie bracket.
  # The Lie advection equation:
  @test decapode"
          Y::DualForm1
          X::Constant

          ∂ₜ(Y) == [X,Y]
        " ==
        @decapode begin
          Y::DualForm1
          X::Constant

          ∂ₜ(Y) == L₁(X,Y)
        end
  # This equation corresponds to diffusion along the contour lines of a vector field X,
  # proportional to the magnitude of X.
  @test decapode"
          Y::DualForm1
          X::Constant

          ∂ₜ(Y) == -1 * [X,[X,Y]]
        " ==
        @decapode begin
          Y::DualForm1
          X::Constant

          ∂ₜ(Y) == -1 * L₁(X,L₁(X,Y))
        end
end
