using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations
using DiagrammaticEquations: Term, Derivative, PlusOperation, MultOperation, Call, Args, Variable, Judgement, Statement, Line, Equation, List, Compose, TypeName

PEG.setdebug!(false) # To disable: PEG.setdebug!(false)

@testset "Terms" begin
    @test Term("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X"))) # Need to specify "DiagrammaticEquations.decapodes" b/c Catlab import also has "Var".
    @test Term("a")[1] == DiagrammaticEquations.decapodes.Var(Symbol("a"))
    @test Term("12")[1] == DiagrammaticEquations.decapodes.Lit(Symbol("12"))
    @test Term("a + b")[1] == DiagrammaticEquations.decapodes.Plus([DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b"))])
end

@testset "Derivatives" begin
   @test Derivative("dt( X )")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
   @test Derivative("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
end

@testset "PlusOperation" begin
    @test PlusOperation("a + b")[1] == DiagrammaticEquations.decapodes.Plus([DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b"))])
    @test PlusOperation("a + b + c")[1] == DiagrammaticEquations.decapodes.Plus([DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b")), DiagrammaticEquations.decapodes.Var(Symbol("c"))])
end

@testset "MultOperation" begin
    @test MultOperation("a * b")[1] == DiagrammaticEquations.decapodes.Mult([DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b"))])
    @test MultOperation("a * b * c")[1] == DiagrammaticEquations.decapodes.Mult([DiagrammaticEquations.decapodes.Var(Symbol("a")), DiagrammaticEquations.decapodes.Var(Symbol("b")), DiagrammaticEquations.decapodes.Var(Symbol("c"))])
end

@testset "Call" begin
    @test Call("a(b)")[1] == App1(:a, DiagrammaticEquations.decapodes.Var(Symbol("b")))
    @test Call("a(b, c)")[1] == App2(:a, DiagrammaticEquations.decapodes.Var(Symbol("b")), DiagrammaticEquations.decapodes.Var(Symbol("c")))
end

@testset "Args" begin
    @test Args("a")[1] == [DiagrammaticEquations.decapodes.Var(:a)]
    @test Args("a, b")[1] == [DiagrammaticEquations.decapodes.Var(:a), DiagrammaticEquations.decapodes.Var(:b)]
    @test Args("∂ₜ(X)")[1] == [Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))]
end

@testset "Variable" begin
    @test Variable("a")[1] == DiagrammaticEquations.decapodes.Var(Symbol("a"))
    @test Variable("you")[1] == DiagrammaticEquations.decapodes.Var(Symbol("you"))
end

@testset "Judgement" begin
    @test Judgement("alpha::beta")[1] == DiagrammaticEquations.decapodes.Judgement(:alpha, :beta, :I)
    @test Judgement("(a, b, c)::d")[1] == [DiagrammaticEquations.decapodes.Judgement(:a, :d, :I),
        DiagrammaticEquations.decapodes.Judgement(:b, :d, :I), DiagrammaticEquations.decapodes.Judgement(:c, :d, :I)]
    @test Judgement("a::b")[1] == DiagrammaticEquations.decapodes.Judgement(:a, :b, :I)
    @test Judgement("X::Y")[1] == DiagrammaticEquations.decapodes.Judgement(:X, :Y, :I)
    @test Judgement("a::Form{X}")[1] == DiagrammaticEquations.decapodes.Judgement(:a, :Form, :X)
end

@testset "Statement" begin
    @test DiagrammaticEquations.Statement("a::b")[1] == DiagrammaticEquations.decapodes.Judgement(:a, :b, :I)
    @test DiagrammaticEquations.Statement("a::b, c::d")[1] != [DiagrammaticEquations.decapodes.Judgement(:a, :b, :I), DiagrammaticEquations.decapodes.Judgement(:c, :d, :I)]
    @test DiagrammaticEquations.Statement("a::b\n")[1] != [DiagrammaticEquations.decapodes.Judgement(:a, :b, :I), DiagrammaticEquations.decapodes.Judgement(:c, :d, :I)]
end

@testset "Line" begin
    @test Line("a::b\n")[1] == DiagrammaticEquations.decapodes.Judgement(:a, :b, :I)
    @test Line("alpha::beta\n")[1] == DiagrammaticEquations.decapodes.Judgement(:alpha, :beta, :I)
    @test Line("x::y\n")[1] == DiagrammaticEquations.decapodes.Judgement(:x, :y, :I)
end

@testset "Equation" begin
    @test Equation("a == b")[1] == DiagrammaticEquations.decapodes.Eq(
        DiagrammaticEquations.decapodes.Var(:a), DiagrammaticEquations.decapodes.Var(:b)
    )
    @test Equation("dt( X ) == Y")[1] == DiagrammaticEquations.decapodes.Eq(
        DiagrammaticEquations.decapodes.Tan(DiagrammaticEquations.decapodes.Var(:X)), 
        DiagrammaticEquations.decapodes.Var(:Y)
    )
end

@testset "List" begin
    @test List("a, b")[1] == [:a, :b]
    @test List("a, b, c")[1] == [:a, :b, :c]
end

@testset "Compose" begin
    @test Compose("∘(a, b)(c)")[1] == AppCirc1([:a, :b], DiagrammaticEquations.decapodes.Var(:c))
    @test Compose("∘(a, b, c)(d)")[1] == AppCirc1([:a, :b, :c], DiagrammaticEquations.decapodes.Var(:d))
end

@testset "TypeName" begin
    @test TypeName("Form0")[1] == :Form0
    @test TypeName("Form0{X}")[1] == [:Form0, :X]
|end