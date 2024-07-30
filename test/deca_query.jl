using Test
using DiagrammaticEquations
using DiagrammaticEquations.Deca
using ACSets

function array_contains_same(test, expected)
    sort(test) == sort(expected)
end

get_index_from_name(d::SummationDecapode, varname::Symbol) = only(incident(d, varname, :name))

@testset "Check sources and targets" begin
  singleton_deca = @decapode begin
    V::infer
  end

  @test !is_var_source(singleton_deca, 1)
  @test !is_var_target(singleton_deca, 1)


  path_op1_deca = @decapode begin
    (X,Z)::infer
    X == d(d(Z)) 
  end

  idxX = get_index_from_name(path_op1_deca, :X)
  idxZ = get_index_from_name(path_op1_deca, :Z)

  @test is_var_source(path_op1_deca, idxZ)
  @test !is_var_target(path_op1_deca, idxZ)

  @test !is_var_source(path_op1_deca, idxX)
  @test is_var_target(path_op1_deca, idxX)


  path_op2_deca = @decapode begin
    X == ∧(Y,Z) 
  end

  idxX = get_index_from_name(path_op2_deca, :X)
  idxY = get_index_from_name(path_op2_deca, :Y)
  idxZ = get_index_from_name(path_op2_deca, :Z)

  idxsYZ = [idxY, idxZ]

  for idx in idxsYZ
    @test is_var_source(path_op2_deca, idx)
    @test !is_var_target(path_op2_deca, idx)
  end
  @test !is_var_source(path_op2_deca, idxX)
  @test is_var_target(path_op2_deca, idxX)


  path_sum_deca = @decapode begin
    X == Y + Z
  end

  idxX = get_index_from_name(path_sum_deca, :X)
  idxY = get_index_from_name(path_sum_deca, :Y)
  idxZ = get_index_from_name(path_sum_deca, :Z)

  idxsYZ = [idxY, idxZ]

  for idx in idxsYZ
    @test is_var_source(path_sum_deca, idx)
    @test !is_var_target(path_sum_deca, idx)
  end
  @test !is_var_source(path_sum_deca, idxX)
  @test is_var_target(path_sum_deca, idxX)

  mixedop_deca = @decapode begin
    Inter == d(X) + ∧(Y, Z)
    Res == d(Inter)
  end

  idxX = get_index_from_name(mixedop_deca, :X)
  idxY = get_index_from_name(mixedop_deca, :Y)
  idxZ = get_index_from_name(mixedop_deca, :Z)
  idxInter = get_index_from_name(mixedop_deca, :Inter)
  idxRes = get_index_from_name(mixedop_deca, :Res)

  @test is_var_source(mixedop_deca, idxX)
  @test is_var_source(mixedop_deca, idxY)
  @test is_var_source(mixedop_deca, idxZ)

  @test is_var_target(mixedop_deca, idxRes)

  @test is_var_target(mixedop_deca, idxInter) && is_var_source(mixedop_deca, idxInter)
end

# TODO: Finish writing these tests
@testset "Get states and terminals" begin
  singleton_deca = @decapode begin
    V::Form1
  end
  @test infer_state_names(singleton_deca) == infer_terminal_names(singleton_deca)

  path_op1_deca = @decapode begin
    (X,Z)::infer
    X == d(d(Z)) 
  end
  @test array_contains_same(infer_state_names(path_op1_deca), [:Z])
  @test array_contains_same(infer_terminal_names(path_op1_deca), [:X])

  path_op2_deca = @decapode begin
    (X,Y,Z)::infer
    X == ∧(Y,Z) 
  end
  @test array_contains_same(infer_state_names(path_op2_deca), [:Y, :Z])
  @test array_contains_same(infer_terminal_names(path_op2_deca), [:X])
end