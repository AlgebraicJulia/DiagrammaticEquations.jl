using Test
using Catlab
using LinearAlgebra
# using MLStyle
using Base.Iterators

using DiagrammaticEquations

@testset "Derivatives" begin
    @test derivative("dt(X)")[1] == ["dt", "(", "X", ")"]
end