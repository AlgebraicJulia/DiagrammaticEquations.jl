using Test
using Catlab
using DiagrammaticEquations
using DiagrammaticEquations: Term, Derivative, PlusOperation, MultOperation, Call, Args,
    Judgement, Statement, Line, Equation, List, Compose, TypeName

PEG.setdebug!(false) # To disable: PEG.setdebug!(false)

# Unit Tests
##############

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
end

@testset "Terms" begin
    @test Term("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X"))) # Need to specify "DiagrammaticEquations.decapodes" b/c Catlab import also has "Var".
    @test Term("a")[1] == DiagrammaticEquations.decapodes.Var(Symbol("a"))
    @test Term("12")[1] == DiagrammaticEquations.decapodes.Lit(Symbol("12"))
    @test Term("∘(a, b)(c)")[1] == AppCirc1([:a, :b], DiagrammaticEquations.decapodes.Var(:c))
    @test Term("a(b)")[1] == App1(:a, DiagrammaticEquations.decapodes.Var(Symbol("b")))
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