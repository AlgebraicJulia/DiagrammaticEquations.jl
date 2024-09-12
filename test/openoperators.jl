using ACSets
using DiagrammaticEquations
using DiagrammaticEquations.Deca
using Test

@testset "Open Operators" begin
  # Error handling
  # --------------

  # Test erroneous left-hand side.
  LHS = @decapode begin
    y == Δ(X) + ∇(Z)
  end
  RHS = @decapode begin
    y == -1*∘(d,⋆,d,⋆)(X)
  end
  Heat = @decapode begin
    ∂ₜ(C) == Δ(C)
  end
  @test_throws "Only single operator replacement is supported for now, but found Op1s: [:Δ, :∇]" replace_op1!(Heat, LHS, RHS)

  # Test erroneous right-hand side.
  LHS = @decapode begin
    y == Δ(X)
  end
  RHS = @decapode begin
    y == -1*∘(d,⋆,d,⋆)(X) + 10*Z
  end
  Heat = @decapode begin
    ∂ₜ(C) == Δ(C)
  end
  @test_throws "The replacement for Δ must have a single input and a single output, but found inputs: [:X, :Z] and outputs [:y]" replace_op1!(Heat, LHS, RHS)

  # Transfering variables
  # ---------------------

  # Test transfering variables on the Brusselator.
  Brusselator = @decapode begin
    (U, V)::Form0
    U2V::Form0
    (U̇, V̇)::Form0
    (α)::Constant
    F::Parameter
    U2V == (U .* U) .* V
    U̇ == 1 + U2V - (4.4 * U) + (α * Δ(U)) + F
    V̇ == (3.4 * U) - U2V + (α * Δ(V))
    ∂ₜ(U) == U̇
    ∂ₜ(V) == V̇
  end
  U_idx = only(incident(Brusselator, :U, :name))
  V_idx = only(incident(Brusselator, :V, :name))
  transfer_children!(Brusselator, U_idx, V_idx)
  @test Brusselator == @decapode begin
    (U, V)::Form0
    U2V::Form0
    (U̇, V̇)::Form0
    (α)::Constant
    F::Parameter
    U2V == (V .* V) .* V
    U̇ == 1 + U2V - (4.4 * V) + (α * Δ(V)) + F
    V̇ == (3.4 * V) - U2V + (α * Δ(V))
    ∂ₜ(V) == U̇
    ∂ₜ(V) == V̇
  end

  # Opening Op1s
  # ------------

  # Test expanding the Heat equation.
  LHS = @decapode begin
    y == Δ(X)
  end
  RHS = @decapode begin
    y == -1*∘(d,⋆,d,⋆)(X)
  end
  Heat = @decapode begin
    ∂ₜ(C) == Δ(C)
  end
  replace_op1!(Heat, LHS, RHS)
  @test Heat == @acset SummationDecapode{Any,Any,Symbol} begin
    Var=4
    type=[:Literal, :infer, :infer, :infer]
    name=[Symbol("-1"), Symbol("•1"), :Ċ, :C]
    TVar=1
    incl=[3]
    Op1=2
    src=[4,4]
    tgt=[3,2]
    op1=[:∂ₜ, [:d, :⋆, :d, :⋆]]
    Op2=1
    proj1=[1]
    proj2=[2]
    res=[3]
    op2=[:*]
  end

  # Test expanding a vector calculus equivalent.
  LHS = @decapode begin
    y == div(X)
  end
  RHS = @decapode begin
    y == ∘(⋆,d,⋆)(X)
  end
  Divergence = @decapode begin
    C::Form0
    V::Form1
    ∂ₜ(C) == C*div(V)
  end
  replace_op1!(Divergence, LHS, RHS)
  @test Divergence == @acset SummationDecapode{Any,Any,Symbol} begin
    Var = 4
    TVar = 1
    Op1 = 2
    Op2 = 1
    src = [1, 4]
    tgt = [3, 2]
    proj1 = [1]
    proj2 = [2]
    res = [3]
    incl = [3]
    op1 = Any[:∂ₜ, [:⋆, :d, :⋆]]
    op2 = [:*]
    type = [:Form0, :infer, :infer, :Form1]
    name = [:C, Symbol("•2"), :Ċ, :V]
  end

  # Test expanding the vector laplacian.
  LHS = @decapode begin
    y == Δ(X)
  end
  RHS = @decapode begin
    y == ∘(d,⋆,d,⋆)(X) + ∘(⋆,d,⋆,d)(X)
  end
  Heat = @decapode begin
    ∂ₜ(C) == -1*Δ(V)
  end
  replace_op1!(Heat, LHS, RHS)
  @test Heat == @acset SummationDecapode{Any,Any,Symbol} begin
    Var = 7
    TVar = 1
    Op1 = 3
    Op2 = 1
    Σ = 1
    Summand = 2
    src = [2, 3, 3]
    tgt = [1, 5, 7]
    proj1 = [4]
    proj2 = [6]
    res = [1]
    incl = [1]
    summand = [7, 5]
    summation = [1, 1]
    sum = [6]
    op1 = Any[:∂ₜ, [:⋆, :d, :⋆, :d], [:d, :⋆, :d, :⋆]]
    op2 = [:*]
    type = [:infer, :infer, :infer, :Literal, :infer, :infer, :infer]
    name = [:Ċ, :C, :V, Symbol("-1"), Symbol("•2"), Symbol("•2"), Symbol("•1")]
  end

  # Test expanding multiple op1s.
  LHS = @decapode begin
    y == Δ(X)
  end
  RHS = @decapode begin
    y == ∘(d,⋆,d,⋆)(X)
  end
  LapLap = @decapode begin
    ∂ₜ(C) == Δ(Δ(C))
  end
  replace_all_op1s!(LapLap, LHS, RHS)
  @test LapLap == @acset SummationDecapode{Any,Any,Symbol} begin
    Var = 3
    TVar = 1
    Op1 = 3
    src = [3, 3, 2]
    tgt = [1, 2, 1]
    incl = [1]
    op1 = Any[:∂ₜ, [:d, :⋆, :d, :⋆], [:d, :⋆, :d, :⋆]]
    type = [:infer, :infer, :infer]
    name = [:Ċ, Symbol("•2"), :C]
  end

  # Test expanding the Brusselator.
  Brusselator = @decapode begin
    (U, V)::Form0
    U2V::Form0
    (U̇, V̇)::Form0
    (α)::Constant
    F::Parameter
    U2V == (U .* U) .* V
    U̇ == 1 + U2V - (4.4 * U) + (α * Δ(U)) + F
    V̇ == (3.4 * U) - U2V + (α * Δ(V))
    ∂ₜ(U) == U̇
    ∂ₜ(V) == V̇
  end
  LHS = @decapode begin
    y == Δ(X)
  end
  RHS = @decapode begin
    y == -1*∘(d,⋆,d,⋆)(X)
  end
  replace_all_op1s!(Brusselator, LHS, RHS)
  @test Brusselator[:op1] == Any[[:d,:⋆,:d,:⋆], [:d,:⋆,:d,:⋆], :∂ₜ, :∂ₜ]

  # Opening Op2s
  # ------------

  # Test expanding the interior product.
  Interior = @decapode begin
    ∂ₜ(C) == ι(A, B)
  end
  LHS = @decapode begin
    y == ι(X, Z)
  end
  RHS = @decapode begin
    y == -1*⋆((⋆p1) ∧ p2)
  end
  replace_all_op2s!(Interior, LHS, RHS)
  @test Interior == @acset SummationDecapode{Any,Any,Symbol} begin
    Var = 8
    TVar = 1
    Op1 = 3
    Op2 = 2
    src = [2, 4, 1]
    tgt = [5, 3, 8]
    proj1 = [7, 3]
    proj2 = [8, 6]
    res = [5, 1]
    incl = [5]
    op1 = [:∂ₜ, :⋆, :⋆]
    op2 = [:*, :∧]
    type = [:infer, :infer, :infer, :infer, :infer, :infer, :Literal, :infer]
    name = [Symbol("•2"), :C, Symbol("•3"), :A, :Ċ, :B, Symbol("-1"), Symbol("•1")]
  end
  Interior = @decapode begin
    ∂ₜ(C) == ι(A, B)
  end
  @test replace_all_op2s!(copy(Interior), LHS, RHS) ==
    replace_all_op2s!(copy(Interior), LHS, RHS,
      only(incident(RHS, :p1, :name)), only(incident(RHS, :p2, :name)))
end

@testset "Rewrite Rules" begin
  # Explicit Rule Application
  # -------------------------

  # Test expanding the Heat equation.
  rule = Op1SDRule(
  @decapode begin
    (X,y,Z)::Form0
    y == Δ(X)
  end
  ,
  @decapode begin
    (X,y)::Form0
    y == -1*∘(d,⋆,d,⋆)(X)
  end)
  Heat = @decapode begin
    C::Form0
    ∂ₜ(C) == Δ(C)
  end
  apply_rule!(Heat, rule)
  @test Heat == @acset SummationDecapode{Any,Any,Symbol} begin
    Var=4
    type=[:infer, :Literal, :Form0, :infer]
    name=[Symbol("•1"), Symbol("-1"), :C, :Ċ]
    TVar=1
    incl=[4]
    Op1=2
    src=[3,3]
    tgt=[4,1]
    op1=[:∂ₜ, [:d, :⋆, :d, :⋆]]
    Op2=1
    proj1=[2]
    proj2=[1]
    res=[4]
    op2=[:*]
  end

  # Test expanding the vector laplacian.
  rule = Op1SDRule(
  @decapode begin
    (X,y)::Form1
    y == Δ(X)
  end
  ,
  @decapode begin
    (X,y)::Form1
    y == ∘(d,⋆,d,⋆)(X) + ∘(⋆,d,⋆,d)(X)
  end)
  VectorHeat = @decapode begin
    V::Form1
    ∂ₜ(V) == -1*Δ(V)
  end
  apply_rule!(VectorHeat, rule)
  @test VectorHeat == @acset SummationDecapode{Any,Any,Symbol} begin
    Var = 6
    TVar = 1
    Op1 = 3
    Op2 = 1
    Σ = 1
    Summand = 2
    src = [5, 5, 5]
    tgt = [2, 3, 1]
    proj1 = [4]
    proj2 = [6]
    res = [2]
    incl = [2]
    summand = [1, 3]
    summation = [1, 1]
    sum = [6]
    op1 = Any[:∂ₜ, [:⋆, :d, :⋆, :d], [:d, :⋆, :d, :⋆]]
    op2 = [:*]
    type = [:infer, :infer, :infer, :Literal, :Form1, :infer]
    name = [Symbol("•1"), :V̇, Symbol("•2"), Symbol("-1"), :V, Symbol("•2")]
  end

  # Default Rules Application
  # -------------------------

  # Test expanding the Brusselator.
  Brusselator = @decapode begin
    (U, V)::Form0
    U2V::Form0
    (U̇, V̇)::Form0
    (α)::Constant
    F::Parameter
    U2V == (U .* U) .* V
    U̇ == 1 + U2V - (4.4 * U) + (α * Δ(U)) + F
    V̇ == (3.4 * U) - U2V + (α * Δ(V))
    ∂ₜ(U) == U̇
    ∂ₜ(V) == V̇
  end
  rewrite!(Brusselator)
  @test Brusselator[:op1] == Any[[:d,:⋆,:d,:⋆], [:d,:⋆,:d,:⋆], :∂ₜ, :∂ₜ]
end

