using Test
using DiagrammaticEquations
using Catlab.ACSetInterface
using Catlab.CategoricalAlgebra

# Downset (ancestor subgraph)
# #########################

# A root variable with no ancestors returns just that variable.
let
  d = @decapode begin
    C::Form0
  end
  e = downset(d, :C)
  @test nparts(e, :Var) == 1
  @test e[1, :name] == :C
  @test nparts(e, :Op1) == 0
  @test nparts(e, :Op2) == 0
  @test nparts(e, :Σ) == 0
end

# Downset of a variable produced by a single Op1 chain.
let
  d = @decapode begin
    B == f(A)
    C == g(B)
  end
  e = downset(d, :C)
  @test nparts(e, :Var) == 3
  @test Set(e[:name]) == Set([:A, :B, :C])
  @test nparts(e, :Op1) == 2
  @test issetequal(e[:op1], [:f, :g])
end

# Downset of an intermediate variable includes only its ancestors.
let
  d = @decapode begin
    B == f(A)
    C == g(B)
  end
  e = downset(d, :B)
  @test nparts(e, :Var) == 2
  @test issetequal(e[:name], [:A, :B])
  @test nparts(e, :Op1) == 1
  @test e[1, :op1] == :f
end

# Downset through an Op2 includes both projections.
let
  d = @decapode begin
    C == A * B
  end
  e = downset(d, :C)
  @test nparts(e, :Var) == 3
  @test issetequal(e[:name], [:A, :B, :C])
  @test nparts(e, :Op2) == 1
  @test e[1, :op2] == :*
end

# Downset through a summation includes all summands.
let
  d = @decapode begin
    D == A + B + C
  end
  e = downset(d, :D)
  @test nparts(e, :Var) == 4
  @test issetequal(e[:name], [:A, :B, :C, :D])
  @test nparts(e, :Σ) == 1
end

# Downset excludes unrelated branches.
let
  d = @decapode begin
    B == f(A)
    D == g(C)
  end
  e = downset(d, :B)
  @test nparts(e, :Var) == 2
  @test issetequal(e[:name], [:A, :B])
  @test nparts(e, :Op1) == 1
end

# Downset with shared ancestors includes them once.
let
  d = @decapode begin
    B == f(A)
    C == g(A)
    D == B * C
  end
  e = downset(d, :D)
  @test nparts(e, :Var) == 4
  @test issetequal(e[:name], [:A, :B, :C, :D])
  @test nparts(e, :Op1) == 2
  @test nparts(e, :Op2) == 1
end

# Combined Op1, Op2, and Σ in the downset.
let
  d = @decapode begin
    F == h(d(A) + f(g(B) * C) + D)
  end
  e = downset(d, :F)
  # F depends on everything.
  @test nparts(e, :Var) == nparts(d, :Var)
  @test nparts(e, :Op1) == nparts(d, :Op1)
  @test nparts(e, :Op2) == nparts(d, :Op2)
  @test nparts(e, :Σ) == nparts(d, :Σ)
end

# Error on nonexistent variable.
let
  d = @decapode begin
    B == f(A)
  end
  @test_throws Exception downset(d, :Z)
end

# Multi-variable downset: union of two disjoint chains.
let
  d = @decapode begin
    B == f(A)
    D == g(C)
  end
  e = downset(d, [:B, :D])
  @test nparts(e, :Var) == 4
  @test issetequal(e[:name], [:A, :B, :C, :D])
  @test nparts(e, :Op1) == 2
  @test issetequal(e[:op1], [:f, :g])
end

# Multi-variable downset: shared ancestor is included once.
let
  d = @decapode begin
    B == f(A)
    C == g(A)
    D == h(E)
  end
  e = downset(d, [:B, :C])
  @test nparts(e, :Var) == 3
  @test issetequal(e[:name], [:A, :B, :C])
  @test nparts(e, :Op1) == 2
end

# Multi-variable downset: single-element vector behaves like the scalar method.
let
  d = @decapode begin
    B == f(A)
    C == g(B)
  end
  e_vec = downset(d, [:C])
  e_sym = downset(d, :C)
  @test nparts(e_vec, :Var) == nparts(e_sym, :Var)
  @test issetequal(e_vec[:name], e_sym[:name])
  @test nparts(e_vec, :Op1) == nparts(e_sym, :Op1)
end

# Multi-variable downset: error when any variable is missing.
let
  d = @decapode begin
    B == f(A)
  end
  @test_throws Exception downset(d, [:B, :Z])
end

# Downset of u from the Navier-Stokes Stream Function / Poisson Problem
# Formulation (Correct) from:
# https://algebraicjulia.github.io/Decapodes.jl/dev/navier_stokes/ns/
#
# The computation graph is:
#   du → ⋆ → •2 → Δ⁻¹ → ψ → d → •4 → ⋆ → u
# and the time derivative branch:
#   u, du → ∧ᵈᵖ₁₀ → •3 → [♭♯,⋆₁,d̃₁] → •1 → (*,-1) → du̇
#
# The downset of u should include only the first chain, excluding the
# time derivative computation, the wedge product, and the literal -1.
let
  ns = @decapode begin
    du::DualForm2
    u::DualForm1
    ψ::Form0

    ψ == Δ⁻¹(⋆(du))
    u == ⋆(d(ψ))

    ∂ₜ(du) == (-1) * ∘(♭♯, ⋆₁, d̃₁)(∧ᵈᵖ₁₀(u, ⋆(du)))
  end

  e = downset(ns, :u)

  # The downset should contain only: du, u, ψ, and two intermediates.
  @test nparts(e, :Var) == 5
  downset_names = Set(e[:name])
  @test :u ∈ downset_names
  @test :du ∈ downset_names
  @test :ψ ∈ downset_names

  # The chain du→⋆→Δ⁻¹→ψ→d→⋆→u uses 4 Op1s.
  @test nparts(e, :Op1) == 4
  @test issetequal(e[:op1], [:⋆, :Δ⁻¹, :d, :⋆])

  # No Op2s (the ∧ᵈᵖ₁₀ and * are excluded).
  @test nparts(e, :Op2) == 0

  # No TVars (the ∂ₜ is excluded).
  @test nparts(e, :TVar) == 0

  # The literal -1 is excluded.
  @test Symbol("-1") ∉ downset_names

  # Sanity: the full decapode has more parts.
  @test nparts(ns, :Var) > nparts(e, :Var)
  @test nparts(ns, :Op1) > nparts(e, :Op1)
  @test nparts(ns, :Op2) > nparts(e, :Op2)

  # Verify there is a monic (injective) ACSet morphism from the downset to the
  # full NS Decapode, proving the downset is a valid inclusion/subobject.
  h = homomorphism(e, ns)
  @test is_natural(h)
  @test is_monic(h)
end
