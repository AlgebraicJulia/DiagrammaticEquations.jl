using Test

using DiagrammaticEquations

# Remove redundant literals
###########################

# No literals:
let
NoLits = @decapode begin
  ∂ₜ(C) == D*Δ(C)
end
@test copy(NoLits) == unique_lits!(NoLits)
end # let

# Distinct literals:
let
DistinctLits = @decapode begin
  ∂ₜ(C) == 5 + -1*D*Δ(C)
end
@test copy(DistinctLits) == unique_lits!(DistinctLits)
end # let

# A repeated literal interacting directly with itself:
let
# XXX: Use the @acset macro, since parse_decapode eliminates repeats already.
#InteractingLits = @decapode begin
#  ∂ₜ(C) == 1*1*D*Δ(C)
#end
InteractingLits = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 8
  TVar = 1
  Op1 = 2
  Op2 = 3
  src = [7, 7]
  tgt = [8, 6]
  proj1 = [1, 3, 5]
  proj2 = [2, 4, 6]
  res = [3, 5, 8]
  incl = [8]
  op1 = [:∂ₜ, :Δ]
  op2 = [:*, :*, :*]
  type = [:Literal, :Literal, fill(:infer, 6)...]
  name = [Symbol("1"), Symbol("1"), :mult_1, :D, :mult_2, Symbol("•2"), :C, :Ċ]
end
@test unique_lits!(InteractingLits) ==
  @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 7
    TVar = 1
    Op1 = 2
    Op2 = 3
    src = [7, 7]
    tgt = [2, 6]
    # Observe that the Literal 1 is multiplied by itself:
    proj1 = [1, 3, 5]
    proj2 = [1, 4, 6]
    res = [3, 5, 2]
    incl = [2]
    op1 = [:∂ₜ, :Δ]
    op2 = [:*, :*, :*]
    type = [:Literal, :infer, :infer, :infer, :infer, :infer, :infer]
    name = [Symbol("1"), :Ċ, :mult_1, :D, :mult_2, Symbol("•2"), :C]
  end
end # let

# A repeated literal used in separate places:
let
ComponentA = @decapode begin
  ∂ₜ(C) == -1*D*Δ(C)
end
ComponentB = @decapode begin
  ∂ₜ(E) == -1*F*Δ(E)
end
RepeatedLits = oplus(ComponentA, ComponentB)

@test unique_lits!(RepeatedLits) ==
  @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 11
  TVar = 2
  Op1 = 4
  Op2 = 4
  src = [2, 2, 8, 8]
  tgt = [1, 5, 7, 11]
  proj1 = [3, 6, 3, 9]
  proj2 = [4, 5, 10, 11]
  res = [6, 1, 9, 7]
  incl = [1, 7]
  op1 = [:∂ₜ, :Δ, :∂ₜ, :Δ]
  op2 = [:*, :*, :*, :*]
  type = [:infer, :infer, :Literal, :infer, :infer, :infer, :infer, :infer, :infer, :infer, :infer]
  name = [:Ċ, :C, Symbol("-1"), :D, Symbol("•2"), :mult_1, :Ė, :E, :mult_1, :F, Symbol("•2")]
  end
end # let

