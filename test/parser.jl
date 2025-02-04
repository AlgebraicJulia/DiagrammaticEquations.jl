using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations


PEG.setdebug!(true)

@testset "Terms" begin
    @test Term("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
end

@testset "Derivatives" begin
   @test Derivative("dt( X )")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
   @test Derivative("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
end

@testset "Call" begin
    @test Call("a(b)")[1] == ["a", "(", "", :b, "", ")"]
end

@testset "Args" begin
    @test Args("a")[1] == :a
    @test Args("a, b")[1] == [:a, :b]
end
