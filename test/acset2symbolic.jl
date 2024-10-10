using Test
using DiagrammaticEquations
using SymbolicUtils: Fixpoint, Prewalk, Postwalk, Chain, @rule
using Catlab

(≃) = is_isomorphic

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
  @test all_ops ≃ all_ops_res

  with_deriv = @decapode begin
    A::Form0
    Ȧ::Form0

    ∂ₜ(A) == Ȧ
    Ȧ == Δ(A)
  end

  @test with_deriv == symbolic_rewriting(with_deriv)

  repeated_vars = @decapode begin
    A::Form0
    B::Form1
    C::Form1

    C == d(A)
    C == Δ(B)
    C == d(A)
  end

  @test repeated_vars == symbolic_rewriting(repeated_vars)

  # TODO: This is broken because of the terminals issue in #77
  self_changing = @decapode begin
    c_exp == ∂ₜ(c_exp)
  end

  @test self_changing == symbolic_rewriting(self_changing)

  literal = @decapode begin
    A::Form0
    B::Form0

    B == A * 2
  end

  @test literal == symbolic_rewriting(literal)

  parameter = @decapode begin
    A::Form0
    P::Parameter
    B::Form0

    B == A * P
  end

  @test parameter == symbolic_rewriting(parameter)

  constant = @decapode begin
    A::Form0
    C::Constant
    B::Form0

    B == A * C
  end

  @test constant == symbolic_rewriting(constant)
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


  distr_d = @decapode begin
    A::Form0
    B::Form1
    C::Form2

    C == d(∧(A, B))
  end
  infer_types!(distr_d)

  leibniz = @rule d(∧(~x, ~y)) => ∧(d(~x), ~y) + ∧(~x, d(~y))

  distr_d_rewritten = symbolic_rewriting(distr_d, expr_rewriter([leibniz]))

  distr_d_res = @decapode begin
    A::Form0
    B::Form1
    C::Form2

    C == ∧(d(A), B) + ∧(A, d(B))
  end
  infer_types!(distr_d_res)

  @test distr_d_res == distr_d_rewritten
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

  @test Heat_open ≃ symbolic_rewriting(Heat, expr_rewriter(rules(Δ, Val(1))))
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

@testset "Literals" begin
    Heat = parse_decapode(quote
        C::Form0
        G::Form0
        ∂ₜ(G) == 3*Δ(C)
    end)
    context = SymbolicContext(Heat)
    SummationDecapode(context)

end

@testset "Parameters" begin

    Heat = @decapode begin
        u::Form0
        G::Form0
        κ::Parameter
        ∂ₜ(G) == Δ(u)*κ
    end
    infer_types!(Heat)

    Heat_open = @decapode begin
        u::Form0
        G::Form0
        κ::Parameter
        ∂ₜ(G) == ★(d(★(d(u))))*κ
    end
    infer_types!(Heat_open)

    Heat_open[7, :name] = Symbol("•4")
    Heat_open[8, :name] = Symbol("•1")

    z = symbolic_rewriting(Heat, expr_rewriter(rules(Δ, Val(1))))
    @test Heat_open ≃ z

end
