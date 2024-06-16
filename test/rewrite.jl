using Test

using DiagrammaticEquations
import DiagrammaticEquations: average_rewrite

using Catlab.ACSetInterface

# Average Rewriting
###################

# No valid rewrites, return original Decapode
DecaTest0 = quote
  A::Form0{X}
  B::Form1{X}
  C::Form2{X}
  
  D::Form0{X}
  E::Form1{X}
  F::Form2{X}

  G::Form0{X}
  H::Form0{X}
  I::Form0{X}
  J::Form0{X}

  B == a(A)
  C == b(B)

  F == f(D, E)

  J == G + H + I
end

Test0 = SummationDecapode(parse_decapode(DecaTest0))
Test0Res = average_rewrite(Test0)
@test Test0 == Test0Res

# Trivial rewrite test
# Make sure var, op names and forms are preserved
DecaTest1 = quote
  D₁::Form0{X}
  D₂::Form1{X}
  F::Form2{X}

  F == c₁(D₁)
  F == c₂(D₂)
end

Test1 = SummationDecapode(parse_decapode(DecaTest1))
Test1Res = average_rewrite(Test1)

Test1Expected = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 7
  type = [:Form0, :Form1, :Form2, :Form2, :Form2, :Form2, :Literal]
  name = [:D₁, :D₂, :F, :oc0_F, :oc1_F, :oc2_F, Symbol("2")]

  Op1 = 2
  src = [1, 2]
  tgt = [4, 5]
  op1 = [:c₁, :c₂]

  Op2 = 1
  proj1 = [6]
  proj2 = [7]
  res = [3]
  op2 = [:/]

  Σ = 1
  sum = [6]

  Summand = 2
  summand = [4, 5]
  summation = [1, 1]
end

@test Test1Res == Test1Expected

# Test with multiple rewrites, op1 only
DecaTest2 = quote
  C₁::Form0{X}
  C₂::Form0{X}
  D₁::Form1{X}
  D₂::Form1{X}
  F::Form1{X}

  D₁ == d₀(C₁)
  D₁ == d₀(C₂)
  F == c₁(D₁)
  F == c₂(D₂)

end

Test2 = SummationDecapode(parse_decapode(DecaTest2))
Test2Res = average_rewrite(Test2)

Test2Expected = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 13

  type = [:Form0, :Form0, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal]

  name = [:C₁, :C₂, :D₁, :D₂, :F, :oc0_D₁, :oc1_D₁, :oc2_D₁, Symbol("2"), :oc0_F, :oc1_F, :oc2_F, Symbol("2")]

  Op1 = 4
  src = [1, 2, 3, 4]
  tgt = [6, 7, 10, 11]
  op1 = [:d₀, :d₀, :c₁, :c₂]

  Op2 = 2
  proj1 = [8, 12]
  proj2 = [9, 13]
  res = [3, 5]
  op2 = [:/, :/]

  Σ = 2
  sum = [8, 12]

  Summand = 4
  summand = [6, 7, 10, 11]
  summation = [1, 1, 2, 2]
end

@test Test2Res == Test2Expected

# Test to ensure rewrites work for op1, op2, and sums
DecaTest3 = quote
  A::Form0{X}
  B::Form0{X}
  C::Form0{X}
  D::Form0{X}
  E::Form1{X}
  F::Form2{X}
  G::Form2{X}

  G == ∧(A, B)
  G == k(C)
  G == t(D)
  G == F + E
end

Test3 = SummationDecapode(parse_decapode(DecaTest3))
Test3Res = average_rewrite(Test3)

Test3Expected = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 13

  type = [:Form0, :Form0, :Form0, :Form0, :Form1, :Form2, :Form2,  :Form2,  :Form2,  :Form2,  :Form2,  :Form2, :Literal]

  name = [:A, :B, :C, :D, :E, :F, :G, :oc0_G, :oc1_G, :oc2_G, :oc3_G, :oc4_G, Symbol("4")]

  Op1 = 2
  src = [3, 4]
  tgt = [8, 9]
  op1 = [:k, :t]

  Op2 = 2
  proj1 = [1, 12]
  proj2 = [2, 13]
  res = [10, 7]
  op2 = [:∧, :/]

  Σ = 2
  sum = [11, 12]

  Summand = 6
  summand = [6, 5, 8, 9, 10, 11]
  summation = [1, 1, 2, 2, 2, 2]
end

@test Test3Res == Test3Expected

# Test to ensure that ops from the same source are all preserved
DecaTest4 = quote
  C::Form0{X}
  D::Form1{X}

  D == k(C)
  D == t(C)
  D == p(C)
end

Test4 = SummationDecapode(parse_decapode(DecaTest4))
Test4Res = average_rewrite(Test4)

Test4Expected = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 7
  type = [:Form0, :Form1, :Form1, :Form1, :Form1, :Form1, :Literal]
  name = [:C, :D, :oc0_D, :oc1_D, :oc2_D, :oc3_D, Symbol("3")]

  Op1 = 3
  src = [1, 1, 1]
  tgt = [3, 4, 5]
  op1 = [:k, :t, :p]

  Op2 = 1
  proj1 = [6]
  proj2 = [7]
  res = [2]
  op2 = [:/]

  Σ = 1
  sum = [6]

  Summand = 3
  summand = [3, 4, 5]
  summation = [1, 1, 1]
end

@test Test4Res == Test4Expected

# Test that larger nary rewrites function properly
DecaTest5 = quote
  A::Form0{X}
  B::Form0{X}
  C::Form1{X}
  D::Form2{X}
  E::Form1{X}
  F::Form0{X}
  G::Form2{X}

  G == f(F)
  G == e(E)
  G == d(D)
  G == c(C)
  G == b(B)
  G == a(A)
end

Test5 = SummationDecapode(parse_decapode(DecaTest5))
Test5Res = average_rewrite(Test5)

Test5Expected = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 15
  type = [:Form0, :Form0, :Form1, :Form2, :Form1, :Form0, 
          :Form2, :Form2, :Form2, :Form2, :Form2, :Form2, :Form2, :Form2, :Literal]
  name = [:A, :B, :C, :D, :E, :F, :G, :oc0_G, :oc1_G, :oc2_G, :oc3_G, :oc4_G, :oc5_G, :oc6_G, Symbol("6")]

  Op1 = 6
  src = [6, 5, 4, 3, 2, 1]
  tgt = [8, 9, 10, 11, 12, 13]
  op1 = [:f, :e, :d, :c, :b, :a]

  Op2 = 1
  proj1 = [14]
  proj2 = [15]
  res = [7]
  op2 = [:/]

  Σ = 1
  sum = [14]

  Summand = 6
  summand = [8, 9, 10, 11, 12, 13]
  summation = [1, 1, 1, 1, 1, 1]
end

@test Test5Res == Test5Expected

# Test multiple rewrites with op2
DecaTest6 = quote
  A::Form0{X}
  B::Form1{X}
  C::Form2{X}
  D::Form0{X}
  E::Form1{X}
  F::Form2{X}

  F == k(A)
  F == t(E)
  E == p(B, C)
  E == q(B, D)
end

Test6 = SummationDecapode(parse_decapode(DecaTest6))
Test6Res = average_rewrite(Test6)

Test6Expected = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 14
  type = [:Form0, :Form1, :Form2, :Form0, :Form1, :Form2, 
  :Form1, :Form1, :Form1, :Literal, :Form2, :Form2, :Form2, :Literal]
  name = [:A, :B, :C, :D, :E, :F, :oc0_E, :oc1_E, :oc2_E, Symbol("2"), :oc0_F, :oc1_F, :oc2_F, Symbol("2")]

  Op1 = 2
  src = [1, 5]
  tgt = [11, 12]
  op1 = [:k, :t]

  Op2 = 4
  proj1 = [2, 2, 9, 13]
  proj2 = [3, 4, 10, 14]
  res = [7, 8, 5, 6]
  op2 = [:p, :q, :/, :/]

  Σ = 2
  sum = [9, 13]

  Summand = 4
  summand = [7, 8, 11, 12]
  summation = [1, 1, 2, 2]
end

@test Test6Res == Test6Expected

# Test multiple rewrites with sums
DecaTest7 = quote
  A::Form0{X}
  B::Form1{X}
  C::Form2{X}
  D::Form1{X}
  E::Form0{X}
  F::Form2{X}

  F == A + B
  F == C + D + E
end

Test7 = SummationDecapode(parse_decapode(DecaTest7))
Test7Res = average_rewrite(Test7)

Test7Expected = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 10
  type = [:Form0, :Form1, :Form2, :Form1, :Form0, :Form2, :Form2, :Form2, :Form2, :Literal]
  name = [:A, :B, :C, :D, :E, :F, :oc0_F, :oc1_F, :oc2_F, Symbol("2")]

  Op2 = 1
  proj1 = [9]
  proj2 = [10]
  res = [6]
  op2 = [:/]

  Σ = 3
  sum = [7, 8, 9]

  Summand = 7
  summand = [1, 2, 3, 4, 5, 7, 8]
  summation = [1, 1, 2, 2, 2, 3, 3]
end

@test Test7Res == Test7Expected

# Test that rewrite ignores forbidden ops, like ∂ₜ
# TODO: This test might break if TVar behavior changes
DecaTest8 = quote 
  D₁::Form1{X}
  D₂::Form2{X}
  F::Form0{X}

  ∂ₜ(D₁) == F
  F == c₂(D₂)
end

Test8 = SummationDecapode(parse_decapode(DecaTest8))
Test8Res = average_rewrite(Test8)

Test8Expected = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 3
  type = [:Form1, :Form2, :Form0]
  name = [:D₁, :D₂, :F]

  TVar = 1
  incl = [3]

  Op1 = 2
  src = [1, 2]
  tgt = [3, 3]
  op1 = [:∂ₜ, :c₂]
end

@test Test8Res == Test8Expected

#=DecaTest9 = quote
  A::Form0{X}
  B::Form0{X}
  C::Form0{X}
  D::Form0{X}
  E::Form2{X}
  F::Form3{X}
  Ḣ::Form5{X}
  H::Form5{X}

  Ḣ == k(B)
  Ḣ == p(D)
  ∂ₜ(H) == Ḣ
end

Test9 = SummationDecapode(parse_decapode(DecaTest9))
Test9Res = average_rewrite(Test9)=#

# Test that rewrites preverse TVars, ignore ∂ₜ
DecaTest10 = quote
  A::Form0{X}
  B::Form0{X}
  C::Form0{X}
  D::Form0{X}
  Ḣ::Form2{X}
  H::Form2{X}

  A == b(B)
  A == d(D)
  Ḣ == c(C)
  ∂ₜ(H) == Ḣ
end

Test10 = SummationDecapode(parse_decapode(DecaTest10))
Test10Res = average_rewrite(Test10)

Test10Expected = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 10
  type = [:Form0, :Form0, :Form0, :Form0, :Form2, :Form2, :Form0, :Form0, :Form0, :Literal]
  name = [:A, :B, :C, :D, :Ḣ, :H, :oc0_A, :oc1_A, :oc2_A, Symbol("2")]

  TVar = 1
  incl = [5]

  Op1 = 4
  src = [2, 4, 3, 6]
  tgt = [7, 8, 5, 5]
  op1 = [:b, :d, :c, :∂ₜ]

  Op2 = 1
  proj1 = [9]
  proj2 = [10]
  res = [1]
  op2 = [:/]

  Σ = 1
  sum = [9]

  Summand = 2
  summand = [7, 8]
  summation = [1, 1]
end

@test Test10Res == Test10Expected

# Test for benchmarking large results, still gives correct output
function makePerfectBinaryDeca(h)
  num_nodes = 2^h - 1
  num_interior = 2^(h-1) - 1
  BinaryTest = @acset SummationDecapode{Any, Any, Symbol} begin
    Var = num_nodes
    type = fill(:Form1, num_nodes)
    name = map(x -> Symbol("•$x"), 1:num_nodes)

    Op1 = 2 * num_interior
    src = vcat(map(x->2*x, 1:num_interior), map(x->2*x+1, 1:num_interior))
    tgt = vcat(1:num_interior, 1:num_interior)
    op1 = map(x -> Symbol("op$x"), 1:(2 * num_interior))

  end
end

h = 5
BinTest = makePerfectBinaryDeca(h)
BinTestRes = average_rewrite(BinTest)

BinTestExpected = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 91
  type = [:Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal, :Form1, :Form1, :Form1, :Literal]
  name = [Symbol("•1"), Symbol("•2"), Symbol("•3"), Symbol("•4"), Symbol("•5"), Symbol("•6"), Symbol("•7"), Symbol("•8"), Symbol("•9"), Symbol("•10"), Symbol("•11"), Symbol("•12"), Symbol("•13"), Symbol("•14"), Symbol("•15"), Symbol("•16"), Symbol("•17"), Symbol("•18"), Symbol("•19"), Symbol("•20"), Symbol("•21"), Symbol("•22"), Symbol("•23"), Symbol("•24"), Symbol("•25"), Symbol("•26"), Symbol("•27"), Symbol("•28"), Symbol("•29"), Symbol("•30"), Symbol("•31"), Symbol("oc0_•1"), Symbol("oc1_•1"), Symbol("oc2_•1"), Symbol("2"), Symbol("oc0_•2"), Symbol("oc1_•2"), Symbol("oc2_•2"), Symbol("2"), Symbol("oc0_•3"), Symbol("oc1_•3"), Symbol("oc2_•3"), Symbol("2"), Symbol("oc0_•4"), Symbol("oc1_•4"), Symbol("oc2_•4"), Symbol("2"), Symbol("oc0_•5"), Symbol("oc1_•5"), Symbol("oc2_•5"), Symbol("2"), Symbol("oc0_•6"), Symbol("oc1_•6"), Symbol("oc2_•6"), Symbol("2"), Symbol("oc0_•7"), Symbol("oc1_•7"), Symbol("oc2_•7"), Symbol("2"), Symbol("oc0_•8"), Symbol("oc1_•8"), Symbol("oc2_•8"), Symbol("2"), Symbol("oc0_•9"), Symbol("oc1_•9"), Symbol("oc2_•9"), Symbol("2"), Symbol("oc0_•10"), Symbol("oc1_•10"), Symbol("oc2_•10"), Symbol("2"), Symbol("oc0_•11"), Symbol("oc1_•11"), Symbol("oc2_•11"), Symbol("2"), Symbol("oc0_•12"), Symbol("oc1_•12"), Symbol("oc2_•12"), Symbol("2"), Symbol("oc0_•13"), Symbol("oc1_•13"), Symbol("oc2_•13"), Symbol("2"), Symbol("oc0_•14"), Symbol("oc1_•14"), Symbol("oc2_•14"), Symbol("2"), Symbol("oc0_•15"), Symbol("oc1_•15"), Symbol("oc2_•15"), Symbol("2")]


  Op1 = 30
  src = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31]
  tgt = [32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89]
  op1 = [Symbol("op1"), Symbol("op2"), Symbol("op3"), Symbol("op4"), Symbol("op5"), Symbol("op6"), Symbol("op7"), Symbol("op8"), Symbol("op9"), Symbol("op10"), Symbol("op11"), Symbol("op12"), Symbol("op13"), Symbol("op14"), Symbol("op15"), Symbol("op16"), Symbol("op17"), Symbol("op18"), Symbol("op19"), Symbol("op20"), Symbol("op21"), Symbol("op22"), Symbol("op23"), Symbol("op24"), Symbol("op25"), Symbol("op26"), Symbol("op27"), Symbol("op28"), Symbol("op29"), Symbol("op30")]

  Op2 = 15
  proj1 = [34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 90]
  proj2 = [35, 39, 43, 47, 51, 55, 59, 63, 67, 71, 75, 79, 83, 87, 91]
  res = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  op2 = [:/, :/, :/, :/, :/, :/, :/, :/, :/, :/, :/, :/, :/, :/, :/]

  Σ = 15
  sum = [34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 90]

  Summand = 30
  summand = [32, 33, 36, 37, 40, 41, 44, 45, 48, 49, 52, 53, 56, 57, 60, 61, 64, 65, 68, 69, 72, 73, 76, 77, 80, 81, 84, 85, 88, 89]
  summation = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15]
end

@test BinTestRes == BinTestExpected

# Remove redundant literals
###########################

@testset "Redundant Literals" begin
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
InteractingLits = @decapode begin
  ∂ₜ(C) == 1*1*D*Δ(C)
end
@test unique_lits!(InteractingLits) ==
  @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 7
    TVar = 1
    Op1 = 2
    Op2 = 3
    src = [2, 2]
    tgt = [1, 5]
    # Observe that the Literal 1 is multiplied by itself:
    proj1 = [3, 6, 7]
    proj2 = [3, 4, 5]
    res = [6, 7, 1]
    incl = [1]
    op1 = [:∂ₜ, :Δ]
    op2 = [:*, :*, :*]
    type = [:infer, :infer, :Literal, :infer, :infer, :infer, :infer]
    name = [:Ċ, :C, Symbol("1"), :D, Symbol("•2") , :mult_1, :mult_2]
  end
end # let

# A repeated literal used in separate places:
let
RepeatedLits = @decapode begin
  ∂ₜ(C) == -1*D*Δ(C)
  ∂ₜ(E) == -1*F*Δ(E)
end
@test unique_lits!(RepeatedLits) ==
  @acset SummationDecapode{Any, Any, Symbol} begin
    Var = 11
    TVar = 2
    Op1 = 4
    Op2 = 4
    src = [2, 2, 9, 9]
    tgt = [1, 5, 8, 11]
    # Observe -1, Var 3, is used in two multiplications:
    proj1 = [3, 6, 3, 7]
    proj2 = [4, 5, 10, 11]
    res = [6, 1, 7, 8]
    incl = [1, 8]
    op1 = [:∂ₜ, :Δ, :∂ₜ, :Δ]
    op2 = [:*, :*, :*, :*]
    type = [:infer, :infer, :Literal, :infer, :infer, :infer, :infer, :infer, :infer, :infer, :infer]
    name = [:Ċ, :C, Symbol("-1"), :D, Symbol("•2"), :mult_1, :mult_2, :Ė, :E, :F, Symbol("•4")]
  end
end # let

end # testset
