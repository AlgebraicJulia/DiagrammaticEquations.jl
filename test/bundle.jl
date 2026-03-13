using Test
using DiagrammaticEquations
using Catlab.ACSetInterface
using Catlab.CategoricalAlgebra

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

# Bundle duplicate Σ (summation) nodes whose summand variable sets are identical.
# Two different equations computing A+B → F and A+B → G: G should be merged into F.
let
  # Σ[1]: F = A + B,  Σ[2]: G = A + B  ← duplicate
  # An Op1 (:foo) uses G downstream.
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var    = 5
    TVar   = 0
    Op1    = 1
    Op2    = 0
    Σ      = 2
    Summand = 4
    src  = [4]        # Op1: src=4(G)
    tgt  = [5]        # Op1: tgt=5(H)
    op1  = [:foo]
    sum      = [3, 4]  # Σ[1]→F, Σ[2]→G
    summand  = [1, 2, 1, 2]   # Summand[1,2] for Σ[1]; Summand[3,4] for Σ[2]
    summation = [1, 1, 2, 2]
    type = [:Form0, :Form0, :Form0, :Form0, :Form0]
    name = [:A, :B, :F, :G, :H]
  end
  e = bundle(d)
  # After bundling: Σ[2] and G removed; the Op1 :foo now uses F as its source.
  @test nparts(e, :Σ)       == 1
  @test nparts(e, :Summand) == 2
  @test nparts(e, :Var)     == 4    # A, B, F, H (G removed)
  @test nparts(e, :Op1)     == 1
  # The remaining Op1 (:foo) must have F as its source.
  @test e[e[:sum][1], :name] == :F
  @test e[e[:src][1], :name] == :F
end

# bundle_sums! is a no-op when no duplicate summations exist.
let
  # Σ[1]: F = A + B,  Σ[2]: G = A + C — different summand sets, no merge.
  NoDupSums = @acset SummationDecapode{Any, Any, Symbol} begin
    Var     = 5      # A=1, B=2, F=3, C=4, G=5
    Σ       = 2
    Summand = 4      # {A,B} for Σ[1]; {A,C} for Σ[2]
    sum       = [3, 5]
    summand   = [1, 2, 1, 4]
    summation = [1, 1, 2, 2]
    type = [:Form0, :Form0, :Form0, :Form0, :Form0]
    name = [:A, :B, :F, :C, :G]
  end
  @test bundle(NoDupSums) == NoDupSums
end

# Idempotency of bundle_sums!
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var    = 4
    Σ      = 2
    Summand = 4
    sum       = [3, 4]
    summand   = [1, 2, 1, 2]
    summation = [1, 1, 2, 2]
    type = [:Form0, :Form0, :Form0, :Form0]
    name = [:A, :B, :F, :G]
  end
  e1 = bundle(d)
  e2 = bundle(e1)
  @test e1 == e2
end

# Homomorphism structure of bundled Decapodes
# ############################################
#
# When bundling removes a duplicate result variable that has NO downstream
# users, the bundled Decapode strictly embeds into the original — there is a
# (strict, monic) ACSet homomorphism  bundle(d) → d.
#
# When the removed variable DID have downstream users that were redirected to
# the kept variable, strict embedding fails (the same kept var now plays two
# roles that differ in the original).  In that case the mathematically correct
# witness is a *loose* quotient-map  d → bundle(d)  that collapses the removed
# variable onto the kept variable.

# Op1 bundling (no downstream use of removed var) — strict embedding ↪ original
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 3
    Op1 = 2
    name = [:h, :dh1, :dh2]
    type = [:Form0, :Form1, :Form1]
    src  = [1, 1]
    tgt  = [2, 3]
    op1  = [:d, :d]
  end
  e = bundle(d)
  @test !isempty(homomorphisms(e, d))
end

# Op2 bundling (no downstream use of removed var) — strict embedding ↪ original
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var  = 4
    Op2  = 2
    name = [:A, :B, :C, :D]
    type = [:Form0, :Form0, :Form0, :Form0]
    proj1 = [1, 1]
    proj2 = [2, 2]
    res   = [3, 4]
    op2   = [:*, :*]
  end
  e = bundle(d)
  @test !isempty(homomorphisms(e, d))
end

# Σ bundling (no downstream use of removed var) — strict embedding ↪ original
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var     = 4
    Σ       = 2
    Summand = 4
    name      = [:A, :B, :F, :G]
    type      = [:Form0, :Form0, :Form0, :Form0]
    sum       = [3, 4]
    summand   = [1, 2, 1, 2]
    summation = [1, 1, 2, 2]
  end
  e = bundle(d)
  @test !isempty(homomorphisms(e, d))
end

# Op1 bundling with downstream redirection — loose quotient-map original → bundled.
# Two duplicate d(h) ops (→ dh1, → dh2); ♯ uses dh2 downstream.
# After bundling dh2 is merged into dh1 and ♯ is redirected.
# A loose homomorphism d → bundle(d) collapses dh2 ↦ dh1.
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 4
    Op1 = 3
    name = [:h, :dh1, :dh2, :v]
    type = [:Form0, :Form1, :Form1, :Form1]
    src  = [1, 1, 3]
    tgt  = [2, 3, 4]
    op1  = [:d, :d, :♯]
  end
  e = bundle(d)
  # The quotient Name-function maps the removed duplicate target to the kept one.
  Name_fn = n -> (n == :dh2 ? :dh1 : n)
  @test is_homomorphic(d, e; type_components=(Name=Name_fn,))
end

# Σ bundling with downstream redirection — loose quotient-map original → bundled.
# Two duplicate Σ (F = A+B and G = A+B); Op1 :foo uses G downstream.
# After bundling G is merged into F and :foo is redirected.
# A loose homomorphism d → bundle(d) collapses G ↦ F.
let
  d = @acset SummationDecapode{Any, Any, Symbol} begin
    Var     = 5
    Op1     = 1
    Σ       = 2
    Summand = 4
    name      = [:A, :B, :F, :G, :H]
    type      = [:Form0, :Form0, :Form0, :Form0, :Form0]
    src       = [4]
    tgt       = [5]
    op1       = [:foo]
    sum       = [3, 4]
    summand   = [1, 2, 1, 2]
    summation = [1, 1, 2, 2]
  end
  e = bundle(d)
  Name_fn = n -> (n == :G ? :F : n)
  @test is_homomorphic(d, e; type_components=(Name=Name_fn,))
end

# Halfar equation — loose quotient-map original → bundled.
# Expanding the Halfar equation produces two duplicate d(h) ops.  After
# bundling, the removed duplicate's target is merged into the kept one.
# A loose homomorphism witnesses the reduction, with the Name-component
# remapping the removed intermediate variable to its kept counterpart.
let
  halfar = @decapode begin
    h::Form0
    Γ::Form1
    n::Constant
    ∂ₜ(h) == ∘(⋆, d, ⋆)(Γ * d(h) ∧ (mag(♯(d(h)))^(n-1)) ∧ (h^(n+2)))
  end
  d = expand_operators(halfar)
  e = bundle(d)
  # Compute the name map: any var in d that was removed by bundling maps to
  # the name of the variable that replaced it (the tgt of the same op in e).
  e_names = Set(e[:name])
  name_map = Dict{Symbol,Symbol}()
  for v in 1:nparts(d, :Var)
    vname = d[v, :name]
    vname ∈ e_names && continue
    for op in incident(d, v, :tgt)
      src_name = d[d[op, :src], :name]
      op_sym   = d[op, :op1]
      for oe in 1:nparts(e, :Op1)
        if e[oe, :op1] == op_sym && e[e[oe, :src], :name] == src_name
          name_map[vname] = e[e[oe, :tgt], :name]
          break
        end
      end
      break
    end
  end
  Name_fn = n -> get(name_map, n, n)
  @test is_homomorphic(d, e; type_components=(Name=Name_fn,))
end

