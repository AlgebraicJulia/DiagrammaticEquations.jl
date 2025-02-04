using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations


PEG.setdebug!(true)

@testset "Derivatives" begin
   @test derivative("dt( X )")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
   @test derivative("∂ₜ(X)")[1] == Tan(DiagrammaticEquations.decapodes.Var(Symbol("X")))
end