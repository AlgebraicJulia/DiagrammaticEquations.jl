using Test
using DiagrammaticEquations
using Catlab.ACSetInterface

# Upset (ancestor subgraph)
# #########################

# A root variable with no ancestors returns just that variable.
let
  d = @decapode begin
    C::Form0
  end
  e = upset(d, :C)
  @test nparts(e, :Var) == 1
  @test e[1, :name] == :C
  @test nparts(e, :Op1) == 0
  @test nparts(e, :Op2) == 0
  @test nparts(e, :Σ) == 0
end

# Upset of a variable produced by a single Op1 chain.
let
  d = @decapode begin
    B == f(A)
    C == g(B)
  end
  e = upset(d, :C)
  @test nparts(e, :Var) == 3
  @test Set(e[:name]) == Set([:A, :B, :C])
  @test nparts(e, :Op1) == 2
  @test Set(e[:op1]) == Set([:f, :g])
end

# Upset of an intermediate variable includes only its ancestors.
let
  d = @decapode begin
    B == f(A)
    C == g(B)
  end
  e = upset(d, :B)
  @test nparts(e, :Var) == 2
  @test Set(e[:name]) == Set([:A, :B])
  @test nparts(e, :Op1) == 1
  @test e[1, :op1] == :f
end

# Upset through an Op2 includes both projections.
let
  d = @decapode begin
    C == A * B
  end
  e = upset(d, :C)
  @test nparts(e, :Var) == 3
  @test Set(e[:name]) == Set([:A, :B, :C])
  @test nparts(e, :Op2) == 1
  @test e[1, :op2] == :*
end

# Upset through a summation includes all summands.
let
  d = @decapode begin
    D == A + B + C
  end
  e = upset(d, :D)
  @test nparts(e, :Var) == 4
  @test Set(e[:name]) == Set([:A, :B, :C, :D])
  @test nparts(e, :Σ) == 1
end

# Upset excludes unrelated branches.
let
  d = @decapode begin
    B == f(A)
    D == g(C)
  end
  e = upset(d, :B)
  @test nparts(e, :Var) == 2
  @test Set(e[:name]) == Set([:A, :B])
  @test nparts(e, :Op1) == 1
end

# Upset with shared ancestors includes them once.
let
  d = @decapode begin
    B == f(A)
    C == g(A)
    D == B * C
  end
  e = upset(d, :D)
  @test nparts(e, :Var) == 4
  @test Set(e[:name]) == Set([:A, :B, :C, :D])
  @test nparts(e, :Op1) == 2
  @test nparts(e, :Op2) == 1
end

# Combined Op1, Op2, and Σ in the upset.
let
  d = @decapode begin
    F == h(d(A) + f(g(B) * C) + D)
  end
  e = upset(d, :F)
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
  @test_throws Exception upset(d, :Z)
end

# Upset of u from the Navier-Stokes Stream Function / Poisson Problem
# Formulation (Correct) from:
# https://algebraicjulia.github.io/Decapodes.jl/dev/navier_stokes/ns/
#
# The computation graph is:
#   du → ⋆ → •2 → Δ⁻¹ → ψ → d → •4 → ⋆ → u
# and the time derivative branch:
#   u, du → ∧ᵈᵖ₁₀ → •3 → [♭♯,⋆₁,d̃₁] → •1 → (*,-1) → du̇
#
# The upset of u should include only the first chain, excluding the
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

  e = upset(ns, :u)

  # The upset should contain only: du, u, ψ, and two intermediates.
  @test nparts(e, :Var) == 5
  upset_names = Set(e[:name])
  @test :u ∈ upset_names
  @test :du ∈ upset_names
  @test :ψ ∈ upset_names

  # The chain du→⋆→Δ⁻¹→ψ→d→⋆→u uses 4 Op1s.
  @test nparts(e, :Op1) == 4
  @test Set(e[:op1]) == Set([:⋆, :Δ⁻¹, :d, :⋆])

  # No Op2s (the ∧ᵈᵖ₁₀ and * are excluded).
  @test nparts(e, :Op2) == 0

  # No TVars (the ∂ₜ is excluded).
  @test nparts(e, :TVar) == 0

  # The literal -1 is excluded.
  @test Symbol("-1") ∉ upset_names

  # Sanity: the full decapode has more parts.
  @test nparts(ns, :Var) > nparts(e, :Var)
  @test nparts(ns, :Op1) > nparts(e, :Op1)
  @test nparts(ns, :Op2) > nparts(e, :Op2)
end
