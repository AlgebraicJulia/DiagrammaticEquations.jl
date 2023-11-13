using Test

@testset "Core" begin
  include("core.jl")
end

@testset "Visualization" begin
  include("migrate/visualization.jl")
end

@testset "Average Rewriting" begin
  include("migrate/rewrite.jl")
end

@testset "Collages" begin
  include("migrate/collages.jl")
end
