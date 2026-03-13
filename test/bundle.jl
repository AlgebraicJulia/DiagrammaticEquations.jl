using Test
using DiagrammaticEquations
using Catlab.ACSetInterface

# Bundle (merge redundant computations)
# ######################################

# No-ops: a Decapode with no duplicate computations is unchanged.
let
  NoDups = @decapode begin
    C::Form0
    D::Form1
    ∂ₜ(C) == Δ(C)
  end
  @test bundle(NoDups) == NoDups
end

# Bundle duplicate Op1s sharing the same source and operator.
# Before: d applied to h twice (→ dh1, → dh2), with ♯ applied to dh2.
# After:  only one d(h); ♯ redirected to use dh1; dh2 removed.
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 4
    TVar = 0
    Op1 = 3
    Op2 = 0
    Σ = 0
    Summand = 0
    # Op1[1]: d,  src=1(h),   tgt=2(dh1)
    # Op1[2]: d,  src=1(h),   tgt=3(dh2)  ← duplicate of Op1[1]
    # Op1[3]: ♯,  src=3(dh2), tgt=4(v)
    src = [1, 1, 3]
    tgt = [2, 3, 4]
    op1 = [:d, :d, :♯]
    type = [:Form0, :Form1, :Form1, :infer]
    name = [:h, :dh1, :dh2, :v]
  end
  e = bundle(d)
  # After bundling: duplicate d(h) is merged; dh2 removed; ♯ uses dh1.
  @test nparts(e, :Op1) == 2
  @test nparts(e, :Var) == 3
  @test e[:op1] == [:d, :♯]
  # ♯ must now use the same variable as d's target
  d_op   = only(findall(==(:d),  e[:op1]))
  sh_op  = only(findall(==(:♯),  e[:op1]))
  @test e[d_op, :tgt] == e[sh_op, :src]
end

# Bundle duplicate Op2s sharing the same source variables and operator.
# Before: A*B computed twice (→ C, → D), then C+D.
# After:  only one A*B; D removed; + uses C for both inputs (C+C).
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 5
    TVar = 0
    Op1 = 0
    Op2 = 3
    Σ = 0
    Summand = 0
    # Op2[1]: *, proj1=1(A), proj2=2(B), res=3(C)
    # Op2[2]: *, proj1=1(A), proj2=2(B), res=4(D)  ← duplicate of Op2[1]
    # Op2[3]: +, proj1=3(C), proj2=4(D), res=5(E)
    proj1 = [1, 1, 3]
    proj2 = [2, 2, 4]
    res = [3, 4, 5]
    op2 = [:*, :*, :+]
    type = [:Form0, :Form0, :Form0, :Form0, :Form0]
    name = [:A, :B, :C, :D, :E]
  end
  e = bundle(d)
  # After bundling: duplicate A*B merged; D removed; + has same var as both inputs.
  @test nparts(e, :Op2) == 2
  @test nparts(e, :Var) == 4
  plus_op = only(findall(==(:+), e[:op2]))
  @test e[plus_op, :proj1] == e[plus_op, :proj2]
end

# Bundle the Halfar equation: d(h) appears twice, bundle should merge them.
let
  halfar = @decapode begin
    h::Form0
    Γ::Form1
    n::Constant
    ∂ₜ(h) == ∘(⋆, d, ⋆)(Γ * d(h) ∧ (mag(♯(d(h)))^(n-1)) ∧ (h^(n+2)))
  end
  d = expand_operators(halfar)
  # After expansion there are exactly 3 :d ops:
  #   two duplicate d(h) applications (same src=h) + one d inside ∘(⋆,d,⋆).
  @test count(==(:d), d[:op1]) == 3
  e = bundle(d)
  # After bundling the two duplicate d(h) ops are merged into one;
  # the d inside ∘(⋆,d,⋆) (different src) remains untouched → 2 :d ops total.
  @test count(==(:d), e[:op1]) == 2
  # The bundled decapode should have fewer Op1s and fewer Vars.
  @test nparts(e, :Op1) < nparts(d, :Op1)
  @test nparts(e, :Var) < nparts(d, :Var)
end

# Idempotency: applying bundle twice gives the same result as once.
let
  halfar = @decapode begin
    h::Form0
    Γ::Form1
    n::Constant
    ∂ₜ(h) == ∘(⋆, d, ⋆)(Γ * d(h) ∧ (mag(♯(d(h)))^(n-1)) ∧ (h^(n+2)))
  end
  d = expand_operators(halfar)
  e1 = bundle(d)
  e2 = bundle(e1)
  @test e1 == e2
end
