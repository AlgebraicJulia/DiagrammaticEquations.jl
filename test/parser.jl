using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations


PEG.setdebug!(true)

@testset "Derivatives" begin
   @test Derivative("dt( X )")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
   @test Derivative("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
end