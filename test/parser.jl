using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations


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

@testset "Call" begin
    @test Call("a(b)")[1] == App1(:a, DiagrammaticEquations.decapodes.Var(Symbol("b")))
end

@testset "Args" begin
    @test Args("a")[1] == [DiagrammaticEquations.decapodes.Var(:a)]
    @test Args("a, b")[1] == [DiagrammaticEquations.decapodes.Var(:a), DiagrammaticEquations.decapodes.Var(:b)]
    @test Args("∂ₜ(X)")[1] == [Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))]
end
