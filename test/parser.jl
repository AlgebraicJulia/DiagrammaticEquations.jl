using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations

PEG.setdebug!(true)

@testset "Derivatives" begin
   @test derivative("dt(X)")[1] == ["dt", "(", "X", ")"]
   @test derivative("∂ₜ(X)")[1] == ["∂ₜ", "(", "X", ")"]
end