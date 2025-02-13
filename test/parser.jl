using Test
using Catlab
using DiagrammaticEquations
using DiagrammaticEquations: Term, Derivative, PlusOperation, MultOperation, Call, Args,
  Judgement, Statement, Line, Equation, List, Compose, TypeName, Grouping, DecapodeExpr

PEG.setdebug!(false) # To disable: PEG.setdebug!(false)

#Taken from "PEG.jl/blob/master/test/misc.jl" to test parsing exception handling
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

# Unit Tests
##############
@testset "DecapodeExpr" begin
  @test DecapodeExpr("a::b\nc == d\ndt(X) == Y\n")[1] == DecaExpr([
  DiagrammaticEquations.decapodes.Judgement(:a, :b, :I)],
  [DiagrammaticEquations.decapodes.Eq(DiagrammaticEquations.decapodes.Var(:c), DiagrammaticEquations.decapodes.Var(:d)),
  DiagrammaticEquations.decapodes.Eq(Tan(DiagrammaticEquations.decapodes.Var(:X)), DiagrammaticEquations.decapodes.Var(:Y))]
  )
  @test DecapodeExpr(" \n")[1] == DecaExpr([],[])
  @test DecapodeExpr("a::b\n")[1] == DecaExpr([
  DiagrammaticEquations.decapodes.Judgement(:a, :b, :I)
  ],[])
end

@testset "Line" begin
  @test Line("a::b\n")[1] == DiagrammaticEquations.decapodes.Judgement(:a, :b, :I)
  @test Line("alpha::beta\n")[1] == DiagrammaticEquations.decapodes.Judgement(:alpha, :beta, :I)
  @test Line("x::y\n")[1] == DiagrammaticEquations.decapodes.Judgement(:x, :y, :I)
end

@testset "Statement" begin
  @test DiagrammaticEquations.Statement("a::b")[1] == DiagrammaticEquations.decapodes.Judgement(:a, :b, :I)
  @test DiagrammaticEquations.Statement("a == b")[1] == DiagrammaticEquations.decapodes.Eq(
  DiagrammaticEquations.decapodes.Var(:a), DiagrammaticEquations.decapodes.Var(:b)
  )
end

@testset "Judgement" begin
  @test Judgement("alpha::beta")[1] == DiagrammaticEquations.decapodes.Judgement(:alpha, :beta, :I)
  @test Judgement("a::Form{X}")[1] == DiagrammaticEquations.decapodes.Judgement(:a, :Form, :X)
  @test Judgement("(a, b, c)::d")[1] == [DiagrammaticEquations.decapodes.Judgement(:a, :d, :I),
  DiagrammaticEquations.decapodes.Judgement(:b, :d, :I), DiagrammaticEquations.decapodes.Judgement(:c, :d, :I)]
  @test Judgement("(a, b, c)::Form{X}")[1] == [DiagrammaticEquations.decapodes.Judgement(:a, :Form, :X),
  DiagrammaticEquations.decapodes.Judgement(:b, :Form, :X), DiagrammaticEquations.decapodes.Judgement(:c, :Form, :X)]
end

@testset "TypeName" begin
  @test TypeName("Form0")[1] == :Form0
  @test TypeName("Form0{X}")[1] == [:Form0, :X]
|end

@testset "Equation" begin
  @test Equation("a == b")[1] == DiagrammaticEquations.decapodes.Eq(
  DiagrammaticEquations.decapodes.Var(:a), DiagrammaticEquations.decapodes.Var(:b)
  )
  @test Equation("a 
  ==
   b")[1] == DiagrammaticEquations.decapodes.Eq(
  DiagrammaticEquations.decapodes.Var(:a), DiagrammaticEquations.decapodes.Var(:b)
  )
  @test Equation("dt( X ) == Y")[1] == DiagrammaticEquations.decapodes.Eq(
  DiagrammaticEquations.decapodes.Tan(DiagrammaticEquations.decapodes.Var(:X)), 
  DiagrammaticEquations.decapodes.Var(:Y)
  )
  @test Equation("f(n+1, N+2) * ∂ₜ(X) == ∘(a, b)(c)")[1] == DiagrammaticEquations.decapodes.Eq(
  DiagrammaticEquations.decapodes.Mult([App2(:f, DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("n")), DiagrammaticEquations.decapodes.Lit(Symbol("1"))]),
  DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("N")), DiagrammaticEquations.decapodes.Lit(Symbol("2"))])),
  Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
  ]),
  AppCirc1([:a, :b], DiagrammaticEquations.decapodes.Var(:c))
  )
end

@testset "MultOperation" begin
  @test MultOperation("a * b")[1] == DiagrammaticEquations.decapodes.Mult([DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b"))])
  @test MultOperation("a * 
  b")[1] == DiagrammaticEquations.decapodes.Mult([DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b"))])
  @test MultOperation("a * b * c")[1] == DiagrammaticEquations.decapodes.Mult(
  [DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b")), DiagrammaticEquations.decapodes.Var(Symbol("c"))])
end

@testset "PlusOperation" begin
  @test PlusOperation("a + b")[1] == DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b"))])
  @test PlusOperation("a + b + c")[1] == DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b")), DiagrammaticEquations.decapodes.Var(Symbol("c"))])
  @test PlusOperation("dt(X) + ∂ₜ(X)")[1] == DiagrammaticEquations.decapodes.Plus(
  [Tan(DiagrammaticEquations.decapodes.Var(Symbol("X"))), Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))])
  @test PlusOperation("a * b + c")[1] == DiagrammaticEquations.decapodes.Plus([
  DiagrammaticEquations.decapodes.Mult([DiagrammaticEquations.decapodes.Var(Symbol("a")), 
  DiagrammaticEquations.decapodes.Var(Symbol("b"))]), DiagrammaticEquations.decapodes.Var(Symbol("c"))])
  @test PlusOperation("3 * (5 + 2)")[1] == DiagrammaticEquations.decapodes.Mult([
  DiagrammaticEquations.decapodes.Lit(Symbol("3")), DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Lit(Symbol("5")), DiagrammaticEquations.decapodes.Lit(Symbol("2"))])
  ])
  @test PlusOperation("3 * 5 + 2")[1] == DiagrammaticEquations.decapodes.Plus([
  DiagrammaticEquations.decapodes.Mult([DiagrammaticEquations.decapodes.Lit(Symbol("3")), DiagrammaticEquations.decapodes.Lit(Symbol("5"))]),
  DiagrammaticEquations.decapodes.Lit(Symbol("2"))
  ])
end

@testset "Terms" begin
  @test Term("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X"))) # Need to specify "DiagrammaticEquations.decapodes" b/c Catlab import also has "Var".
  @test Term("a")[1] == DiagrammaticEquations.decapodes.Var(Symbol("a"))
  @test Term("12")[1] == DiagrammaticEquations.decapodes.Lit(Symbol("12"))
  @test Term("∘(a, b)(c)")[1] == AppCirc1([:a, :b], DiagrammaticEquations.decapodes.Var(:c))
  @test Term("a(b)")[1] == App1(:a, DiagrammaticEquations.decapodes.Var(Symbol("b")))
end

@testset "Grouping" begin
  @test Grouping("(a)")[1] == DiagrammaticEquations.decapodes.Var(Symbol("a"))
  @test Grouping("(a + b)")[1] == DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b"))])
end

@testset "Derivatives" begin
   @test Derivative("dt( X )")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
   @test Derivative("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
end

@testset "Compose" begin
  @test Compose("∘(a, b)(c)")[1] == AppCirc1([:a, :b], DiagrammaticEquations.decapodes.Var(:c))
  @test Compose("∘(a, b, c)(d)")[1] == AppCirc1([:a, :b, :c], DiagrammaticEquations.decapodes.Var(:d))
  @test Compose("∘(a)(∂ₜ(X))")[1] == AppCirc1([:a], Tan(DiagrammaticEquations.decapodes.Var(Symbol("X"))))
end

@testset "Call" begin
  @test Call("a(b)")[1] == App1(:a, DiagrammaticEquations.decapodes.Var(Symbol("b")))
  @test Call("a(b, c)")[1] == App2(:a, DiagrammaticEquations.decapodes.Var(Symbol("b")), DiagrammaticEquations.decapodes.Var(Symbol("c")))
  @test Call("f(n + 1)")[1] == App1(:f, DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("n")), DiagrammaticEquations.decapodes.Lit(Symbol("1"))]))
  @test Call("f(n+1, N+2)")[1] == App2(:f, DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("n")), DiagrammaticEquations.decapodes.Lit(Symbol("1"))]),
  DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("N")), DiagrammaticEquations.decapodes.Lit(Symbol("2"))]))
end

@testset "Args" begin
  @test Args("a")[1] == [DiagrammaticEquations.decapodes.Var(:a)]
  @test Args("a, b")[1] == [DiagrammaticEquations.decapodes.Var(:a), DiagrammaticEquations.decapodes.Var(:b)]
  @test Args("∂ₜ(X)")[1] == [Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))]
  @test Args("∂ₜ(X), dt(Y)")[1] == [Tan(DiagrammaticEquations.decapodes.Var(Symbol("X"))), Tan(DiagrammaticEquations.decapodes.Var(Symbol("Y")))]
  @test Args("n + 1, n + 2")[1] == [DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("n")), DiagrammaticEquations.decapodes.Lit(Symbol("1"))]),
  DiagrammaticEquations.decapodes.Plus(
  [DiagrammaticEquations.decapodes.Var(Symbol("n")), DiagrammaticEquations.decapodes.Lit(Symbol("2"))])]
end

@testset "List" begin
  @test List("a, b")[1] == [:a, :b]
  @test List("a, b, c")[1] == [:a, :b, :c]
end

# END TO END Tests
##################

(≃) = is_isomorphic

@testset "End to End" begin
  parsed_result = decapode"
  (C, Ċ)::Form0
  ϕ::Form1
  ϕ ==  ∘(k, d₀)(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ"

  DiffusionExprBody1 = quote
  (C, Ċ)::Form0
  ϕ::Form1
  ϕ ==  ∘(k, d₀)(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ
  end
  ddp1 = SummationDecapode(parse_decapode(DiffusionExprBody1))
  
  @test parsed_result ≃ ddp1
  
  # Support parsing literals. # Swamps an infered part. Represents identical. Database slightly constructed differently.
  parsed_result = decapode"
  (C, Ċ)::Form0
  ϕ::Form1
  ϕ ==  2*d₀(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ"

  DiffusionExprBody2 =  quote
  (C, Ċ)::Form0
  ϕ::Form1
  ϕ ==  2*d₀(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ
  end

  ddp2 = SummationDecapode(parse_decapode(DiffusionExprBody2))

  @test parsed_result ≃ ddp2
  
  # TODO - ^^^^ Problem: Our parser interperts '2*d₀(C)' as multiplication. Original parser interperets as function call with 2 as argument:
  #  App2(:*, Lit(2), App1(:d₀, Var(:C))). Looks like in their pattern matching switch case, App2 comes before Mult and Julia
  # already interperts 2*d₀(C) as a function call where * is the call and lhs and rhs are arguments.
  # We need to change precedences?

  # Multiply without explicitly giving parentheses.
  parsed_result = decapode"
  (C, Ċ)::Form0
  ϕ::Form1
  ϕ ==  4*2*3*d₀(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ"

  parse_decapode(quote ϕ ==  4*2*3*d₀(C) end)

  DiffusionExprBody3 =  quote
  (C, Ċ)::Form0
  ϕ::Form1
  ϕ ==  4*2*3*d₀(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ
  end

  ddp3 = SummationDecapode(parse_decapode(DiffusionExprBody3))

  @test parsed_result ≃ ddp3

  # Variables need not be declared before use.
  parsed_result = decapode"
  Ċ::Form0
  ϕ ==  4*2*3*d₀(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ"

  DiffusionExprBody4 =  quote
  Ċ::Form0
  ϕ ==  4*2*3*d₀(C)
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ
  end
  
  ddp4 = SummationDecapode(parse_decapode(DiffusionExprBody4))
  
  @test parsed_result ≃ ddp4

  parsed_result = decapode"
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ"
  
  DiffusionExprBody5 =  quote
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end

  ddp5 = SummationDecapode(parse_decapode(DiffusionExprBody5))

  @test parsed_result ≃ ddp5

  # TVars can be parsed on either side of an equation.
  parsed_result = decapode"
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    Ċ == ∂ₜ(C)"

  DiffusionExprBody6 =  quote
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    Ċ == ∂ₜ(C)
  end

  ddp6 = SummationDecapode(parse_decapode(DiffusionExprBody6))

  @test parsed_result ≃ ddp6

  parsed_result = decapode"
    ϕ ==  2*d₀(C)
    ∂ₜ(C) == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)"

  # Because vars are infered, the two differ in names although they are identical in
  # structure. I have modified the names to match so that the test focuses on structure.
  parsed_result[:name] = [:ϕ, Symbol("2"), Symbol("•2"), :C, :Ċ]

  DiffusionExprBody7 =  quote
    ϕ ==  2*d₀(C)
    ∂ₜ(C) == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  end

  ddp7 = SummationDecapode(parse_decapode(DiffusionExprBody7))

  @test parsed_result ≃ ddp7

  # Vars can only be of certain types.
  @test_throws ErrorException parsed_result = decapode"
    (C)::Foo
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    Ċ == ∂ₜ(C)"
  
  # Just noting that the first decapode is denotes a X as an op1
  # while the second is a multiplication between X and F
  parsed_result_1 = decapode"
    (A, B, X)::Form0{X}
    A == X(F)"

    ParseTest2_1 = quote
      (A, B, X)::Form0{X}
      A == X(F)
    end
    pt2_1 = SummationDecapode(parse_decapode(ParseTest2_1))

    @test parsed_result_1 ≃ pt2_1

    # TODO This has not been implemented and does not neccessarily need to be implemented right now -> Multiplication by Parenthesis. 
    # parsed_result_2 = decapode"
    #   (A, B, X)::Form0{X}
    #   A == (X)F"

    # ParseTest2_2 = quote
    #   (A, B, X)::Form0{X}
    #   A == (X)F
    # end

    pt2_2 = SummationDecapode(parse_decapode(ParseTest2_2))

    @test parsed_result_2 ≃ pt2_2 

    @test parsed_result_1 != parsed_result_2
  
    # Chained Tvars test
    # TODO: Do we want explict support for higher order Tvars?
    parsed_result = decapode"
      D == ∂ₜ(C)
      E == ∂ₜ(D)"

    ParseTest3 = quote
      D == ∂ₜ(C)
      E == ∂ₜ(D)
    end

    pt3 = SummationDecapode(parse_decapode(ParseTest3))

    @test parsed_result ≃ pt3

    # Do not rename TVars if they are given a name.
    parsed_result = decapode"
      X::Form0{Point}
      V::Form0{Point}
  
      k::Constant{Point}
  
      ∂ₜ(X) == V
      ∂ₜ(V) == -1*k*(X)"

    pt5 = SummationDecapode(parse_decapode(quote
      X::Form0{Point}
      V::Form0{Point}
  
      k::Constant{Point}
  
      ∂ₜ(X) == V
      ∂ₜ(V) == -1*k*(X)
    end))

    @test parsed_result ≃ pt5
end

# Exception Handling Tests
##########################

@testset "Exception Handling" begin
    # Multiple equality is not an accepted input
  test_input = "(A, B, X)::Form0{X}
    A == d(B) == f(X)\n"

  @test parse_fails_at(DecapodeExpr, test_input) == 35
end