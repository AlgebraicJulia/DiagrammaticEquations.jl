using Test

include("pretty.jl")

@testset "Core" begin
  include("core.jl")
end

@testset "Composition" begin
  include("composition.jl")
end

@testset "Language" begin
  include("language.jl")
end

@testset "Visualization" begin
  include("visualization.jl")
end

@testset "Average Rewriting" begin
  include("rewrite.jl")
end

@testset "Redundant Literals" begin
  include("redundant_lits.jl")
end

@testset "Collages" begin
  include("collages.jl")
end

@testset "SummationDecapode Deconstruction" begin
  include("colanguage.jl")
end

@testset "Open Operators" begin
  include("openoperators.jl")
end

@testset "Symbolic Rewriting" begin
  include("graph_traversal.jl")
  include("acset2symbolic.jl")
end

@testset "ThDEC Symbolics" begin
  include("decasymbolic.jl")
end

# include("aqua.jl")
