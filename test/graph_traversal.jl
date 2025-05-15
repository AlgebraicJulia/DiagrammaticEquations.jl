using DiagrammaticEquations
using ACSets
using MLStyle
using Test

function is_correct_length(d::SummationDecapode, result)
  return length(result) == n_ops(d)
end

@testset "Topological Sort on Edges" begin
  no_edge = @decapode begin
    F == S
  end
  @test isempty(topological_sort_edges(no_edge))

  one_op1_deca = @decapode begin
    F == f(S)
  end
  result = topological_sort_edges(one_op1_deca)
  @test is_correct_length(one_op1_deca, result)
  @test retrieve_name(one_op1_deca, only(result)) == :f

  multi_op1_deca = @decapode begin
    F == c(b(a(S)))
  end
  result = topological_sort_edges(multi_op1_deca)
  @test is_correct_length(multi_op1_deca, result)
  for (edge, test_name) in zip(result, [:a, :b, :c])
    @test retrieve_name(multi_op1_deca, edge) == test_name
  end

  cyclic = @decapode begin
    B == g(A)
    A == f(B)
  end
  @test_throws AssertionError topological_sort_edges(cyclic)

  just_op2 = @decapode begin
    C == A * B
  end
  result = topological_sort_edges(just_op2)
  @test is_correct_length(just_op2, result)
  @test retrieve_name(just_op2, only(result)) == :*

  just_simple_sum = @decapode begin
    C == A + B
  end
  result = topological_sort_edges(just_simple_sum)
  @test is_correct_length(just_simple_sum, result)
  @test retrieve_name(just_simple_sum, only(result)) == :+

  just_multi_sum = @decapode begin
    F == A + B + C + D + E
  end
  result = topological_sort_edges(just_multi_sum)
  @test is_correct_length(just_multi_sum, result)
  @test retrieve_name(just_multi_sum, only(result)) == :+

  op_combo = @decapode begin
    F == h(d(A) + f(g(B) * C) + D)
  end
  result = topological_sort_edges(op_combo)
  @test is_correct_length(op_combo, result)
end
