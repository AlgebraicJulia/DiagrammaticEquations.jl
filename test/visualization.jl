using Test
using DiagrammaticEquations
using ACSets
using Catlab.Graphics
using Catlab.Graphics.Graphviz
using Catlab.Graphs.PropertyGraphs

DecaTest = quote
    (A, B, C, D)::Form0{X}
  
    D == k(A + B) + p(C + B)
  end
  
Test1 = SummationDecapode(parse_decapode(DecaTest))
t1 = to_graphviz(Test1)
@test Graphviz.filter_statements(t1, Graphviz.Edge, :label) == ["k", "p", "+", "+", "+"]
@test Graphviz.filter_statements(t1, Graphviz.Node, :label) == [ "A:Ω₀", "B:Ω₀", "C:Ω₀", "D:Ω₀", "•1:Ω•", "sum_1:Ω•", "•2:Ω•", "sum_2:Ω•", "", "", "Σ1", "Σ2", "Σ3" ]
@test Graphviz.filter_statements(t1, Graphviz.Node, :shape) == [ "none", "none", "circle", "circle", "circle"]
# Test that the default attributes are correct.
@test t1.graph_attrs == Dict(:rankdir => "TB")
@test t1.node_attrs == Dict(:height => "0.05", :margin => "0", :shape => "oval", :width => "0.05")

t1_attributes = to_graphviz(Test1, edge_attrs=Dict(:color => "cornflowerblue"), node_attrs=Dict(:shape => "egg"), graph_attrs=Dict(:rankdir => "LR"))
# Test that the default attributes are overwritten only where specified.
@test t1_attributes.edge_attrs == Dict(:arrowsize => "0.5", :color => "cornflowerblue")
@test t1_attributes.node_attrs == Dict(:height => "0.05", :margin => "0", :shape => "egg", :width => "0.05")
@test t1_attributes.graph_attrs == Dict(:rankdir => "LR")
# Test that the per-edge and per-node attributes are not overwritten.
@test Graphviz.filter_statements(t1_attributes, Graphviz.Edge, :label) == ["k", "p", "+", "+", "+"]
@test Graphviz.filter_statements(t1_attributes, Graphviz.Node, :label) == [ "A:Ω₀", "B:Ω₀", "C:Ω₀", "D:Ω₀", "•1:Ω•", "sum_1:Ω•", "•2:Ω•", "sum_2:Ω•", "", "", "Σ1", "Σ2", "Σ3" ]
@test Graphviz.filter_statements(t1_attributes, Graphviz.Node, :shape) == [ "none", "none", "circle", "circle", "circle"]

t1_no_default_changes = to_graphviz(Test1, node_attrs=Dict(:color => "red"), graph_attrs=Dict(:bgcolor => "fuchsia"))
# Test that the default attributes are not overwritten when not specified.
# (In this case, there should be no overwriting of default node shape, and graph rankdir.)
@test t1_no_default_changes.node_attrs == Dict(:color => "red", :height => "0.05", :margin => "0", :shape => "oval", :width => "0.05")
@test t1_no_default_changes.graph_attrs == Dict(:bgcolor => "fuchsia", :rankdir => "TB")

DecaTest2 = quote
  A::Form0{X}
  B::Form0{X}
  C::Form0{X}
  D::Form0{X}
  Ḋ::Form0{X}

  Ḋ == k(A, B) + p(C, B) - 1
  ∂ₜ(D) == Ḋ 
end

Test2 = SummationDecapode(parse_decapode(DecaTest2))
t2 = to_graphviz(Test2)
@test Graphviz.filter_statements(t2, Graphviz.Edge, :label) == ["∂ₜ", "π₁", "π₂", "k", "π₁", "π₂", "p", "π₁", "π₂", "-", "+"]
@test Graphviz.filter_statements(t2, Graphviz.Node, :label) == ["A:Ω₀", "B:Ω₀", "C:Ω₀", "D:Ω₀", "Ḋ:Ω₀", "1:ΩL", "•1:Ω•", "•2:Ω•", "sum_1:Ω•", "", "", "Ω₀×Ω₀", "Ω₀×Ω₀", "Ω•×ΩL", "Σ1"]
@test Graphviz.filter_statements(t2, Graphviz.Node, :shape) == ["none", "none", "rectangle", "rectangle", "rectangle", "circle"]

t2_undirected = to_graphviz(Test2, directed = false)
@test Graphviz.filter_statements(t2_undirected, Graphviz.Edge, :label) == ["∂ₜ", "π₁", "π₂", "k", "π₁", "π₂", "p", "π₁", "π₂", "-", "+"]
@test Graphviz.filter_statements(t2_undirected, Graphviz.Node, :label) == ["A:Ω₀", "B:Ω₀", "C:Ω₀", "D:Ω₀", "Ḋ:Ω₀", "1:ΩL", "•1:Ω•", "•2:Ω•", "sum_1:Ω•", "Ω₀×Ω₀", "Ω₀×Ω₀", "Ω•×ΩL", "Σ1"]
@test Graphviz.filter_statements(t2_undirected, Graphviz.Node, :shape) == ["rectangle", "rectangle", "rectangle", "circle"]

Test3 = SummationDecapode(parse_decapode(quote
           A::Form0
           B::Form1
           C::Form2
           D::DualForm0
           E::DualForm1
           F::DualForm2
           G::Literal
           H::Parameter
           I::Constant
           J::infer
           end))

t3 = to_graphviz(Test3)
@test Graphviz.filter_statements(t3, Graphviz.Node, :label) == ["A:Ω₀", "B:Ω₁", "C:Ω₂", "D:Ω̃₀", "E:Ω̃₁", "F:Ω̃₂", "G:ΩL", "H:ΩP", "I:ΩC", "J:Ω•", "", ""]

# Same decapode as Test2 but with all names the same
Test3 = @acset SummationDecapode{Any, Any, Symbol}  begin
  Var = 7
  type = Any[:Form0, :Form0, :Form0, :Form0, :infer, :infer, :infer]
  name = [:A, :A, :A, :A, :A, :A, :A]

  TVar = 1
  incl = [5]

  Op1 = 1
  src = [4]
  tgt = [5]
  op1 = Any[:∂ₜ]

  Op2 = 2
  proj1 = [1, 3]
  proj2 = [2, 2]
  res = [6, 7]
  op2 = Any[:k, :p]

  Σ = 1
  sum = [5]

  Summand = 2
  summand = [6, 7]
  summation = [1, 1]
end

t3 = to_graphviz(Test3, verbose = true)
@test Graphviz.filter_statements(t3, Graphviz.Edge, :label) == ["∂ₜ", "π₁", "π₂", "k", "π₁", "π₂", "p", "+"]
@test Graphviz.filter_statements(t3, Graphviz.Node, :label) == ["A:Ω₀", "A:Ω₀", "A:Ω₀", "A:Ω₀", "A:Ω•", "A:Ω•", "A:Ω•", "", "", "Ω₀×Ω₀", "Ω₀×Ω₀", "Σ1"]
@test Graphviz.filter_statements(t3, Graphviz.Node, :shape) == ["none", "none", "rectangle", "rectangle", "circle"]

Test4 = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 5
  type = [:Form0, :Form0, :Form0, :Form0, :Form0]
  name = [Symbol("_A"), :A, Symbol("_•"), Symbol("•_"), Symbol("_")]
end

t4 = to_graphviz(Test4, directed = false, verbose = false)
@test Graphviz.filter_statements(t4, Graphviz.Node, :label) == ["A:Ω₀", "A:Ω₀", "•:Ω₀", ":Ω₀", ":Ω₀"]

t5 = to_graphviz(Test4, directed = false, verbose = true)
@test Graphviz.filter_statements(t5, Graphviz.Node, :label) == ["_A:Ω₀", "A:Ω₀", "_•:Ω₀", "•_:Ω₀", "_:Ω₀"]

Test6 = SummationDecapode(parse_decapode(quote
           A::Form0
           B::Form1
           C::Form2
           D::DualForm0
           E::DualForm1
           F::DualForm2
           G::Literal
           H::Parameter
           I::Constant
           J::infer
           end))

t6 = to_graphviz(Test6)
@test Graphviz.filter_statements(t6, Graphviz.Node, :label) == ["A:Ω₀", "B:Ω₁", "C:Ω₂", "D:Ω̃₀", "E:Ω̃₁", "F:Ω̃₂", "G:ΩL", "H:ΩP", "I:ΩC", "J:Ω•", "", ""]

AlteredKlausmeier = @acset SummationDecapode{Any, Any, Symbol} begin
  Var = 21
  TVar = 2
  Op1 = 3
  Op2 = 10
  Σ = 2
  Summand = 4
  src = [1, 1, 2]
  tgt = [4, 10, 14]
  proj1 = [1, 2, 3, 6, 12, 1, 2, 16, 11, 13]
  proj2 = [8, 7, 1, 9, 2, 19, 18, 17, 2, 21]
  res = [7, 6, 9, 5, 16, 18, 17, 15, 21, 20]
  incl = [4, 14]
  summand = [5, 10, 15, 20]
  summation = [1, 1, 2, 2]
  sum = [4, 14]
  op1 = [:∂ₜ, :Δ, :∂ₜ]
  op2 = [:^, :*, :*, :-, :-, :^, :*, :-, :L, :*]
  type = [:DualForm0, :DualForm0, :Constant, :infer, :infer, :infer, :infer, :Literal, :infer, :infer, :Form1, :Constant, :Constant, :infer, :infer, :infer, :infer, :infer, :Literal, :infer, :infer]
  name = [:sum_N, :W, :phyto_m, :phyto_ṅ, Symbol("phyto_•2"), Symbol("phyto_•3"), Symbol("phyto_•4"), Symbol("2"), Symbol("phyto_•5"), Symbol("phyto_•6"), :hydro_dX, :hydro_a, :hydro_ν, :hydro_ẇ, Symbol("hydro_•2"), Symbol("hydro_•3"), Symbol("hydro_•4"), Symbol("hydro_•5"), Symbol("2"), Symbol("hydro_•6"), Symbol("hydro_•7")]
end

t7 = to_graphviz(AlteredKlausmeier, color = true)
@test length(unique(Graphviz.filter_statements(t7, Graphviz.Node, :fillcolor))) == 3

