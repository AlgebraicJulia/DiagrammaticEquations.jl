using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations: Term, Derivative, PlusOperation, MultOperation, Call, Args, List, Compose



PEG.setdebug!(true) # To disable: PEG.setdebug!(false)

@testset "Terms" begin
    @test Term("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X"))) # Need to specify "DiagrammaticEquations.decapodes" b/c Catlab import also has "Var".
    @test Term("a")[1] == DiagrammaticEquations.decapodes.Var(Symbol("a"))
    @test Term("12")[1] == DiagrammaticEquations.decapodes.Lit(Symbol("12"))
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

@testset "List" begin
    @test List("a, b")[1] == ["a", "b"]
    @test List("a, b, c")[1] == ["a", "b", "c"]
end

@testset "Compose" begin
    @test Compose("∘(a, b)(c)")[1] == AppCirc1([:a, :b], DiagrammaticEquations.decapodes.Var(:c))
    @test Compose("∘(a, b, c)(d)")[1] == AppCirc1([:a, :b, :c], DiagrammaticEquations.decapodes.Var(:d))
end