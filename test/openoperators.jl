using ACSets
using DiagrammaticEquations
using Test

@testset "Open Operators" begin
  LHS = @decapode begin
    y == Δ(X)
  end
  RHS = @decapode begin
    y == -1*∘(d,⋆,d,⋆)(X)
  end
  Heat = @decapode begin
    ∂ₜ(C) == Δ(C)
  end
  replace_operators!(Heat, LHS, RHS)
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
end
