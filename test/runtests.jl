using Test

include("pretty.jl")

@testset "Core" begin
  include("core.jl")
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

@testset "Collages" begin
  include("collages.jl")
end

@testset "SummationDecapode Deconstruction" begin
  include("colanguage.jl")
end
