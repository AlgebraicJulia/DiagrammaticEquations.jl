using Test
using DiagrammaticEquations
using DiagrammaticEquations.Deca
using Catlab

# import DiagrammaticEquations: OpenSummationDecapode, Open, oapply, oapply_rename

# @testset "Composition" begin
# Simplest possible decapode relation.
Trivial = @decapode begin
  H::Form0{X}
end

trivial_relation = @relation () begin
  trivial(H)
end

otrivial = Open(Trivial, [:H])
apex_original = apex(otrivial)
deep_copies = deepcopy(otrivial)
trivial_comp_from_vector = oapply(trivial_relation, [otrivial])
trivial_comp_from_single = oapply(trivial_relation, otrivial)

@test trivial_relation(otrivial) == apex(trivial_comp_from_single)

# Test the oapply is correct.
@test apex(trivial_comp_from_vector)    == Trivial
@test apex(trivial_comp_from_single) == Trivial
# Test none of the decapodes were mutated
@test isequal(otrivial, deep_copies)
# Test that we did not change where the apex of the OpenSummationDecapode points to.
@test apex_original === apex(otrivial)


# Multiple variables and an equation.
AdvectionExprBody =  quote
  C::Form0{X}
  (V, ϕ)::Form1{X}
  ϕ == ∧₀₁(C,V)
end
advExpr = parse_decapode(AdvectionExprBody)
Advection = SummationDecapode(advExpr)
adv_relation = @relation () begin
  advection(C,V,ϕ)
end
adv_comp = oapply(adv_relation, [Open(Advection, [:C,:V,:ϕ])])

adv_comp_expected = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 3
  type = [:Form0, :Form1, :Form1]
  name = [:C, :V, :ϕ]
  Op2 = 1
  proj1 = [1]
  proj2 = [2]
  res = [3]
  op2 = [:∧₀₁]
end
@test adv_relation(Open(Advection, [:C, :V, :ϕ])) == adv_comp_expected
@test apex(adv_comp) == adv_comp_expected

# This is the example from the "Overview" page in the docs.
DiffusionExprBody =  quote
  C::Form0{X}
  ϕ::Form1{X}

  # Fick's first law
  ϕ ==  ∘(k, d₀)(C)
end
AdvectionExprBody = quote
  C::Form0{X}
  (V, ϕ)::Form1{X}

  ϕ == ∧₀₁(C,V)
end
SuperpositionExprBody = quote
  (C, Ċ)::Form0{X}
  (ϕ, ϕ₁, ϕ₂)::Form1{X}

  ϕ == ϕ₁ + ϕ₂
  Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  ∂ₜ(C) == Ċ
end

difExpr = parse_decapode(DiffusionExprBody)
Diffusion = SummationDecapode(difExpr)

advExpr = parse_decapode(AdvectionExprBody)
Advection = SummationDecapode(advExpr)

supExpr = parse_decapode(SuperpositionExprBody)
#Superposition = SummationDecapode(supExpr)
Superposition = SummationDecapode(supExpr)

compose_diff_adv = @relation (C,V) begin
  diffusion(C, ϕ₁)
  advection(C, ϕ₂, V)
  superposition(ϕ₁, ϕ₂, ϕ, C)
end

decapodes_vars = [
  Open(Diffusion, [:C, :ϕ]),
  Open(Advection, [:C, :ϕ, :V]),
  Open(Superposition, [:ϕ₁, :ϕ₂, :ϕ, :C])]
#debugg = oapply_rename(compose_diff_adv, decapodes_vars)

original_apexes = map(apex, decapodes_vars)
deep_copies = deepcopy(decapodes_vars) # This is to test none of the decapodes are mutated.

dif_adv_sup = oapply(compose_diff_adv, decapodes_vars)
@test compose_diff_adv(decapodes_vars) == apex(dif_adv_sup)

dif_adv_sup_expected = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 6
  type = [:Form0, :Form1, :Form1, :Form1, :Form0, :Form1]
  name = [:C, :ϕ₁, :V, :ϕ₂, :superposition_Ċ, :ϕ]

  TVar = 1
  incl = [5]

  Op1 = 3
  src = [1,6,1]
  tgt = [2,5,5]
  op1 = [[:k, :d₀], [:⋆₀⁻¹, :dual_d₁, :⋆₁], :∂ₜ]

  Op2 = 1
  proj1 = [1]
  proj2 = [3]
  res = [4]
  op2 = [:∧₀₁]

  Σ = 1
  sum = [6]

  Summand = 2
  summand = [2,4]
  summation = [1,1]
end
@test apex(dif_adv_sup) == dif_adv_sup_expected

# Test none of the decapodes were mutated
@test isequal(decapodes_vars, deep_copies)
# Test that we did not change where the apexes of the OpenSummationDecapodes point to.
@test all(original_apexes .=== map(apex, decapodes_vars))

# Test some other permutation of the symbols yields the same decapode.
compose_diff_adv = @relation (C,V) begin
  diffusion(C, ϕ₁)
  #advection(C, ϕ₂, V)
  #superposition(ϕ₁, ϕ₂, ϕ, C)
  advection(V, ϕ₂, C)
  superposition(ϕ₁, ϕ₂, C, ϕ)
end

decapodes_vars = [
  Open(Diffusion, [:C, :ϕ]),
  #Open(Advection, [:C, :ϕ, :V]),
  #Open(Superposition, [:ϕ₁, :ϕ₂, :ϕ, :C])]
  Open(Advection, [:V, :ϕ, :C]),
  Open(Superposition, [:ϕ₁, :ϕ₂, :C, :ϕ])]
dif_adv_sup = oapply(compose_diff_adv, decapodes_vars)

@test apex(dif_adv_sup) == dif_adv_sup_expected

# Test that expand_operators doesn't break composition.
AdvectionExprBody = quote
  C::Form0{X}
  (V, ϕ)::Form1{X}
  ϕ == ∧₀₁(C,V)
end

advExpr = parse_decapode(AdvectionExprBody)

Advection = expand_operators(SummationDecapode(advExpr))

self_adv = @relation () begin
  advection₁(C,V,ϕ)
  advection₂(C,V,ϕ)
end


adv_adv = [
 Open(Advection, [:C,:V,:ϕ]),
 Open(Advection, [:C,:V,:ϕ])]
deep_copies = deepcopy(adv_adv) # This is to test none of the decapodes are mutated.
adv_adv_comp = oapply(self_adv, adv_adv)
adv_adv_comp_expected = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 3
  type = [:Form0, :Form1, :Form1]
  name = [:C, :V, :ϕ]
  Op2 = 2
  proj1 = [1,1]
  proj2 = [2,2]
  res = [3,3]
  op2 = [:∧₀₁,:∧₀₁]
end
@test apex(adv_adv_comp) == adv_adv_comp_expected
# Test none of the decapodes were mutated
@test isequal(adv_adv, deep_copies)

@testset "Default Composition Diagrams" begin
  # Compose Diffusion, Advection, and Superposition.
  # Observe that we have pre-named ϕ to ϕ₁ or ϕ₂.
  # Note that only one Decapode contains TVars.
  Diffusion = @decapode begin
    C::Form0
    ϕ₁::Form1
    ϕ₁ ==  ∘(k, d₀)(C)
  end
  Advection = @decapode begin
    C::Form0
    (V, ϕ₂)::Form1
    ϕ₂ == ∧₀₁(C,V)
  end
  Superposition = @decapode begin
    (C, Ċ)::Form0
    (ϕ, ϕ₁, ϕ₂)::Form1
    ϕ == ϕ₁ + ϕ₂
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  expected =
    @relation () begin
      Diffusion(C,ϕ₁)
      Advection(C,V,ϕ₂)
      Superposition(C,ϕ₁,ϕ₂,Ċ,ϕ)
    end
  @test is_isomorphic(expected, default_composition_diagram(
          [Diffusion, Advection, Superposition],
          [:Diffusion, :Advection, :Superposition]))

  # Compose Halfar's equation with Glen's law.
  GlensLaw = @decapode begin
    Γ::Form1
    (A,ρ,g,n)::Constant

    Γ == (2/(n+2))*A*(ρ*g)^n
  end
  HalfarsEquation = @decapode begin
    h::Form0
    Γ::Form1
    n::Constant

    ∂ₜ(h) == ∘(⋆, d, ⋆)(Γ  * d(h) ∧ (mag(♯(d(h)))^(n-1)) ∧ (h^(n+2)))
  end
  expected =
    @relation () begin
      GlensLaw(Γ,A,ρ,g,n)
      HalfarsEquation(h,Γ,n,ḣ)
    end
  @test is_isomorphic(expected, default_composition_diagram(
          [GlensLaw, HalfarsEquation],
          [:GlensLaw, :HalfarsEquation],
          true))

  # Test an error is thrown when # of models ≠ # of names.
  Diffusion = @decapode begin
    C::Form0
    ϕ₁::Form1
    ϕ₁ ==  ∘(k, d₀)(C)
  end
  Advection = @decapode begin
    C::Form0
    (V, ϕ₂)::Form1
    ϕ₂ == ∧₀₁(C,V)
  end
  Superposition = @decapode begin
    (C, Ċ)::Form0
    (ϕ, ϕ₁, ϕ₂)::Form1
    ϕ == ϕ₁ + ϕ₂
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  @test_throws "3 models given, but 2 names provided." default_composition_diagram(
    [Diffusion, Advection, Superposition],
    [:Diffusion, :Advection])

  # Test an error is thrown when a single model contains duplicate names.
  Diffusion = @decapode begin
    C::Form0
    ϕ₁::Form1
    ϕ₁ ==  ∘(k, d₀)(C)
  end
  AdvectionDuplicates = @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 4
    type = [:Form0, :Form1, :Form1, :Form1]
    name = [:C, :V, :ϕ₂, :ϕ₂]
    Op2 = 1
    proj1 = [1]
    proj2 = [2]
    res = [3]
    op2 = [:∧₀₁]
  end
  Superposition = @decapode begin
    (C, Ċ)::Form0
    (ϕ, ϕ₁, ϕ₂)::Form1
    ϕ == ϕ₁ + ϕ₂
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  @test_throws "Decapode AdvectionDuplicates contains repeated variable names: Set([:ϕ₂])." default_composition_diagram(
    [Diffusion, AdvectionDuplicates, Superposition],
    [:Diffusion, :AdvectionDuplicates, :Superposition])
end

# end
