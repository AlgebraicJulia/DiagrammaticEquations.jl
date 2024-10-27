using Test
using DiagrammaticEquations
using Catlab

# TODO: Special names for morphisms that match a method of grabbing boundary
# simplices.

# TODO: Initial conditions.
# - This will mean that we need to return both a masked Decapode, and a means of pluggin initial data in for physical quantities, replacing `constuct`.
# TODO: Temporal boundary conditions.
# TODO: General boundaries i.e. arbitrary functions of solutions.

# TODO: Add test with empty boundary Decapode.

# Note: Since the order does not matter in which rb1 and rb2 are applied, it
# seems informal to state that one goes before the other.
# It might be better to provide a semantics for incident edges a la:
#Diffusion = @Decapode begin
#  C::Form0
#  ∂ₜ(C) == rb3(∘(d,⋆,d,⋆)(rb1(C)))
#  ∂ₜ(C) == rb3(∘(d,⋆,d,⋆)(rb2(C)))
#end
# Such a technique would preserve the technical definition of "collage".


# Test simple boundary masks.
DiffusionDynamics = infer_types!(@decapode begin
  K::Form0
  ∂ₜ(K) == ∘(d,⋆,d,⋆)(K)
end)
DiffusionBoundaries = @decapode begin
  (Kb1, Kb2, Null)::Form0
end
DiffusionMorphism = @relation () begin
  rb1_leftwall(C, Cb1)
  rb2_rightwall(C, Cb2)
  rb3(Ċ, Zero)
end
DiffusionSymbols = Dict(
  :C => :K,
  :Ċ => :K̇,
  :Cb1 => :Kb1,
  :Cb2 => :Kb2,
  :Zero => :Null)
DiffusionCollage = DiagrammaticEquations.collate(
  DiffusionDynamics,
  DiffusionBoundaries,
  DiffusionMorphism,
  DiffusionSymbols)

@test DiffusionCollage == @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 8
  TVar = 1
  Op1 = 2
  Op2 = 3
  src = [1, 3]
  tgt = [7, 2]
  proj1 = [5, 1, 2]
  proj2 = [4, 6, 8]
  res = [3, 5, 7]
  incl = [2]
  op1 = Any[:∂ₜ, [:d, :⋆, :d, :⋆]]
  op2 = [:rb1_leftwall, :rb2_rightwall, :rb3]
  type = [:Form0, :Form0, :Form0, :Form0, :Form0, :Form0, :Form0, :Form0]
  name = [:K, :K̇, :r1_K, :Kb1, :r2_K, :Kb2, :r3_K̇, :Null]
end

# Test boundary condition applications on intermediate variables.
IntermediateDynamics = infer_types!(@decapode begin
  K::Form0
  J == Δ(K)
  ∂ₜ(K) == Δ(J)
end)
IntermediateBoundaries = @decapode begin
  (Jb1)::Form0
end
IntermediateMorphism = @relation () begin
  rb1_leftwall(C, Cb1)
end
IntermediateSymbols = Dict(
  :C => :J,
  :Cb1 => :Jb1)
IntermediateCollage = DiagrammaticEquations.collate(
  IntermediateDynamics,
  IntermediateBoundaries,
  IntermediateMorphism,
  IntermediateSymbols)
@test IntermediateCollage == @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 5
  TVar = 1
  Op1 = 3
  Op2 = 1
  src = [1, 1, 4]
  tgt = [2, 3, 3]
  proj1 = [2]
  proj2 = [5]
  res = [4]
  incl = [3]
  op1 = [:Δ, :∂ₜ, :Δ]
  op2 = [:rb1_leftwall]
  type = [:Form0, :Form0, :Form0, :Form0, :Form0]
  name = [:K, :J, :K̇, :r1_J, :Jb1]
end

# Test twice-applied boundary condition applications on intermediate variables.
TwiceDynamics = infer_types!(@decapode begin
  K::Form0
  J == Δ(K)
  ∂ₜ(K) == Δ(J)
end)
TwiceBoundaries = @decapode begin
  (Jb1, Jb2)::Form0
end
TwiceMorphism = @relation () begin
  rb1_leftwall(C, Cb1)
  rb2_rightwall(C, Cb2)
end
TwiceSymbols = Dict(
  :C => :J,
  :Cb1 => :Jb1,
  :Cb2 => :Jb2)
TwiceCollage = DiagrammaticEquations.collate(
  TwiceDynamics,
  TwiceBoundaries,
  TwiceMorphism,
  TwiceSymbols)
@test TwiceCollage == @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 7
  TVar = 1
  Op1 = 3
  Op2 = 2
  src = [1, 1, 4]
  tgt = [2, 3, 3]
  proj1 = [6, 2]
  proj2 = [5, 7]
  res = [4, 6]
  incl  = [3]
  op1 = [:Δ, :∂ₜ, :Δ]
  op2 = [:rb1_leftwall, :rb2_rightwall]
  type = [:Form0, :Form0, :Form0, :Form0, :Form0, :Form0, :Form0]
  name = [:K, :J, :K̇, :r1_J, :Jb1, :r2_J, :Jb2]
end

# Test that `restrictions` adds the correct restriction edges
DiffusionCollage = DiagrammaticEquations.collate(
  DiffusionDynamics,
  DiffusionBoundaries,
  DiffusionMorphism,
  DiffusionSymbols;
  restrictions=Dict(:Kb1 => :K))

@test DiffusionCollage == @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 8
  TVar = 1
  Op1 = 3
  Op2 = 3
  src = [1, 3, 5]
  tgt = [7, 2, 4]
  proj1 = [5, 1, 2]
  proj2 = [4, 6, 8]
  res = [3, 5, 7]
  incl = [2]
  op1 = Any[:∂ₜ, [:d, :⋆, :d, :⋆], :restrict_Kb1]
  op2 = [:rb1_leftwall, :rb2_rightwall, :rb3]
  type = [:Form0, :Form0, :Form0, :Form0, :Form0, :Form0, :Form0, :Form0]
  name = [:K, :K̇, :r1_K, :Kb1, :r2_K, :Kb2, :r3_K̇, :Null]
end

# Parameters
# Test simple boundary masks.
ParamDiffusionDynamics = infer_types!(@decapode begin
  K::Form0
  A::Parameter
  ∂ₜ(K) == A*(∘(d,⋆,d,⋆)(K))
end)
ParamDiffusionBoundaries = @decapode begin
  (Kb1, Kb2, Null)::Form0
  Ab::Parameter
end
ParamDiffusionMorphism = @relation () begin
  rb1_leftwall(C, Cb1)
  rb2_rightwall(C, Cb2)
  rb3(Ċ, Zero)
  r0(Param,BoundaryParam)
end
ParamDiffusionSymbols = Dict(
  :C => :K,
  :Ċ => :K̇,
  :Param => :A,
  :BoundaryParam => :Ab,
  :Cb1 => :Kb1,
  :Cb2 => :Kb2,
  :Zero => :Null)
ParamDiffusionCollage = DiagrammaticEquations.collate(
  ParamDiffusionDynamics,
  ParamDiffusionBoundaries,
  ParamDiffusionMorphism,
  ParamDiffusionSymbols)
infer_types!(ParamDiffusionCollage)

@test ParamDiffusionCollage == @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 12
  TVar = 1
  Op1 = 2
  Op2 = 5
  src = [1, 5]
  tgt = [9, 4]
  proj1 = [11, 7, 1, 3, 2]
  proj2 = [4, 6, 8, 10, 12]
  res = [3, 5, 7, 9, 11]
  incl = [3]
  op1 = Any[:∂ₜ, [:d, :⋆, :d, :⋆]]
  op2 = [:*, :rb1_leftwall, :rb2_rightwall, :rb3, :r0]
  type = [:Form0, :Parameter, :Form0, :infer, :Form0, :Form0, :Form0, :Form0, :Form0, :Form0, :Parameter, :Parameter]
  name = [:K, :A, :K̇, Symbol("•2"), :r1_K, :Kb1, :r2_K, :Kb2, :r3_K̇, :Null, :r4_A, :Ab]
end

