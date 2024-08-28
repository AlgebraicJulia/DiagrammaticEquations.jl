using DiagrammaticEquations
using ACSets
using MLStyle
using Test

function is_topo_sort_ordered(result::AbstractVector{TableData})
  seen_edges = Dict{Symbol, Int}(:Op1 => 0, :Op2 => 0, :Σ => 0)
  for entry in result
    table = entry.table_name
    prev_seen = seen_edges[table]
    if !(prev_seen < entry.table_index)
      return false
    end
    seen_edges[table] = entry.table_index
  end
  return true
end

@testset "Topological Sort on Edges" begin
  no_edge = @decapode begin
    F == S
  end
  @test isempty(topological_sort_edges(no_edge))

  one_op1 = @decapode begin
    F == f(S)
  end
  result = topological_sort_edges(one_op1)
  @test retrieve_name(one_op1, only(result)) == :f
  @test is_topo_sort_ordered(result)

  multi_op1 = @decapode begin
    F == c(b(a(S)))
  end
  result = topological_sort_edges(multi_op1)
  for (edge, test_name) in zip(result, [:a, :b, :c])
    @test retrieve_name(multi_op1, edge) == test_name
  end
  @test is_topo_sort_ordered(result)

  cyclic = @decapode begin
    B == g(A)
    A == f(B)
  end
  @test_throws AssertionError topological_sort_edges(cyclic)

  just_op2 = @decapode begin
    C == A * B
  end
  result = topological_sort_edges(just_op2)
  @test retrieve_name(just_op2, only(result)) == :*
  @test is_topo_sort_ordered(result)

  just_simple_sum = @decapode begin
    C == A + B
  end
  result = topological_sort_edges(just_simple_sum)
  @test retrieve_name(just_simple_sum, only(result)) == :+
  @test is_topo_sort_ordered(result)

  just_multi_sum = @decapode begin
    F == A + B + C + D + E
  end
  result = topological_sort_edges(just_multi_sum)
  @test retrieve_name(just_multi_sum, only(result)) == :+
  @test is_topo_sort_ordered(result)

  op_combo = @decapode begin
    F == h(d(A) + f(g(B) * C) + D)
  end
  result = topological_sort_edges(op_combo)
  @test is_topo_sort_ordered(result)

  sum_with_single_dependency = @decapode begin
    F == A + f(A) + h(g(A))
  end
  result = topological_sort_edges(sum_with_single_dependency)
  @test is_topo_sort_ordered(result)
end
