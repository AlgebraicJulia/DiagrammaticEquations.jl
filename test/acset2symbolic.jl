using Test
using DiagrammaticEquations
using SymbolicUtils: Fixpoint, Prewalk, Postwalk, Chain, @rule
using Catlab

@testset "Basic Roundtrip" begin
  op1_only = @decapode begin
    A::Form0
    B::Form1
    B == d(A)
  end

  @test op1_only == symbolic_rewriting(op1_only)


  op2_only = @decapode begin
    A::Constant
    B::Form0
    C::Form0

    C == A * B
  end

  @test op2_only == symbolic_rewriting(op2_only)

  sum_only = @decapode begin
    A::Form2
    B::Form2
    C::Form2

    C == A + B
  end

  @test sum_only == symbolic_rewriting(sum_only)


  multi_sum = @decapode begin
    A::Form2
    B::Form2
    C::Form2
    D::Form2

    D == (A + B) + C
  end
  infer_types!(multi_sum)

  # TODO: This is correct but the symbolics is splitting up the sum
  @test multi_sum == symbolic_rewriting(multi_sum)


  all_ops = @decapode begin
    A::Constant
    B::Form0
    C::Form1
    D::Form1
    E::Form1
    F::Form1

    C == d(B)
    D == A * C
    F == D + E
  end

  # This loses the intermediate names C and D
  all_ops_res = symbolic_rewriting(all_ops)
  all_ops_res[5, :name] = :D
  all_ops_res[6, :name] = :C
  @test is_isomorphic(all_ops,all_ops_res)
end

function expr_rewriter(rules::Vector)
  return Fixpoint(Prewalk(Fixpoint(Chain(rules))))
end

@testset "Basic Rewriting" begin
  op1s = @decapode begin
    A::Form0
    B::Form2
    C::Form2

    C == B + B + d(d(A))
  end

  dd_0 = @rule d(d(~x)) => 0

  op1s_rewritten = symbolic_rewriting(op1s, expr_rewriter([dd_0]))

  op1s_equiv = @decapode begin
    A::Form0
    B::Form2
    C::Form2

    C == 2 * B
  end

  @test op1s_equiv == op1s_rewritten


  op2s = @decapode begin
    A::Form0
    B::Form0
    C::Form0
    D::Form0


    D == ∧(∧(A, B), C)
  end

  wdg_assoc = @rule ∧(∧(~x, ~y), ~z) => ∧(~x, ∧(~y, ~z))

  op2s_rewritten = symbolic_rewriting(op2s, expr_rewriter([wdg_assoc]))

  op2s_equiv = @decapode begin
    A::Form0
    B::Form0
    C::Form0
    D::Form0


    D == ∧(A, ∧(B, C))
  end
  infer_types!(op2s_equiv)

  @test op2s_equiv == op2s_rewritten

end

@testset "Heat" begin
  Heat = @decapode begin
    C::Form0
    G::Form0
    D::Constant
    ∂ₜ(G) == D*Δ(C)
  end
  infer_types!(Heat)

  # Same up to re-naming
  Heat[5, :name] = Symbol("•1")
  @test Heat == symbolic_rewriting(Heat)

  Heat_open = @decapode begin
    C::Form0
    G::Form0
    D::Constant
    ∂ₜ(G) == D*★(d(★(d(C))))
  end
  infer_types!(Heat_open)

  Heat_open[8, :name] = Symbol("•1")
  Heat_open[5, :name] = Symbol("•2")
  Heat_open[6, :name] = Symbol("•3")
  Heat_open[7, :name] = Symbol("•4")

  @test is_isomorphic(Heat_open, symbolic_rewriting(Heat, expr_rewriter(rules(Δ, Val(1)))))
end

@testset "Phytodynamics" begin
  Phytodynamics = @decapode begin
    (n,w)::Form0
    m::Constant
    ∂ₜ(n) == w + m*n + Δ(n)
  end
  infer_types!(Phytodynamics)
  test_phy = symbolic_rewriting(Phytodynamics)
end
