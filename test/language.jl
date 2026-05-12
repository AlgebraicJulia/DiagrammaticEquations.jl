using Test
using Catlab.ACSetInterface
using Catlab.CategoricalAlgebra
using LinearAlgebra
using MLStyle
using Base.Iterators

using DiagrammaticEquations
using DiagrammaticEquations.Deca

import DiagrammaticEquations: Judgement, filterfor_ec_types, Var

@testset "Parsing" begin
  # Tests
  #######

  # Construct roughly what the @decapode macro should return for Diffusion
  js = [Judgement(:C, :Form0, :X),
        Judgement(:Ċ₁, :Form0, :X),
        Judgement(:Ċ₂, :Form0, :X)
  ]
  # TODO: Do we need to handle the fact that all the functions are parameterized by a space?
  eqs = [Eq(Var(:Ċ₁), AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁, :k, :d₀], Var(:C))),
         Eq(Var(:Ċ₂), AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁, :d₀], Var(:C))),
         Eq(Tan(Var(:C)), Plus([Var(:Ċ₁), Var(:Ċ₂)]))
  ]
  diffusion_d = DecaExpr(js, eqs)
  # diffusion_cset = Decapode(diffusion_d)
  diffusion_cset_named = SummationDecapode(diffusion_d)
  # A test with expressions on LHS (i.e. temporary variables must be made)
  # TODO: we should be intelligent and realize that the LHS of the first two
  # equations are the same and so can share a new variable
  eqs = [Eq(Plus([Var(:Ċ₁), Var(:Ċ₂)]), AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁, :k, :d₀], Var(:C))),
         Eq(Plus([Var(:Ċ₁), Var(:Ċ₂)]), AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁, :d₀], Var(:C))),
         Eq(Tan(Var(:C)), Plus([Var(:Ċ₁), Var(:Ċ₂)]))
  ]
  test_d = DecaExpr(js, eqs)
  # test_cset = Decapode(test_d)
  test_cset_named = SummationDecapode(test_d)

  # TODO: Write tests for recursive expressions

  all(isassigned(test_cset_named[:name], i) for i in parts(test_cset_named,:Var))

  sup_js = js = [Judgement(:C, :Form0, :X),
  Judgement(:ϕ₁, :Form0, :X),
  Judgement(:ϕ₂, :Form0, :X)
  ]
  sup_eqs = [Eq(Var(:ϕ₁), AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁, :k, :d₀], Var(:C))),
         Eq(Var(:ϕ₂), AppCirc1([:⋆₀⁻¹, :dual_d₁, :⋆₁, :d₀], Var(:C))),
         Eq(Tan(Var(:C)), Plus([Var(:ϕ₁), Var(:ϕ₂)]))
  ]
  sup_d = DecaExpr(sup_js, sup_eqs)
  # sup_cset = Decapode(sup_d)
  sup_cset_named = SummationDecapode(sup_d)


  # Decapodes.compile(diffusion_cset_named, [:C,])
  # Decapodes.compile(test_cset_named, [:C,])
  # Decapodes.compile(sup_cset_named, [:C,])

  term(:(∧₀₁(C,V)))

  # No need to parameterize forms over a space (i.e. {X} syntax)
  DiffusionExprBody1 =  quote
    (C, Ċ)::Form0
    ϕ::Form1
    ϕ ==  ∘(k, d₀)(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  diffExpr1 = parse_decapode(DiffusionExprBody1)
  ddp1 = SummationDecapode(diffExpr1)
  @test ddp1[:name] == [:C, :Ċ, :ϕ]

  # Support parsing literals.
  DiffusionExprBody2 =  quote
    (C, Ċ)::Form0
    ϕ::Form1
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  diffExpr2 = parse_decapode(DiffusionExprBody2)
  ddp2 = SummationDecapode(diffExpr2)
  @test ddp2[:name] == [:C, :Ċ, :ϕ, Symbol("•",1), Symbol(2)]

  # Multiply without explicitly giving parentheses.
  DiffusionExprBody3 =  quote
    (C, Ċ)::Form0
    ϕ::Form1
    ϕ ==  4*2*3*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  diffExpr3 = parse_decapode(DiffusionExprBody3)
  ddp3 = SummationDecapode(diffExpr3)
  @test ddp3[:name] == [:C, :Ċ, :ϕ, Symbol("4"), Symbol("2"), Symbol("3"), Symbol("•1"), :mult_1, :mult_2]

  # Variables need not be declared before use.
  DiffusionExprBody4 =  quote
    Ċ::Form0
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  diffExpr4 = parse_decapode(DiffusionExprBody4)
  ddp4 = SummationDecapode(diffExpr4)
  DiffusionExprBody5 =  quote
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    ∂ₜ(C) == Ċ
  end
  diffExpr5 = parse_decapode(DiffusionExprBody5)
  ddp5 = SummationDecapode(diffExpr5)
  @test ddp5[:name] == [:ϕ, :Ċ, Symbol("2"), Symbol("•1"), :C]

  # TVars can be parsed on either side of an equation.
  DiffusionExprBody6 =  quote
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    Ċ == ∂ₜ(C)
  end
  diffExpr6 = parse_decapode(DiffusionExprBody6)
  ddp6 = SummationDecapode(diffExpr6)
  @test ddp6[:name] == [:ϕ, :Ċ, Symbol("2"), Symbol("•1"), :C]
  @test ddp6[:incl] == [2]
  DiffusionExprBody7 =  quote
    ϕ ==  2*d₀(C)
    ∂ₜ(C) == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
  end
  diffExpr7 = parse_decapode(DiffusionExprBody7)
  ddp7 = SummationDecapode(diffExpr7)
  @test ddp7[:name] == [:ϕ, :Ċ, Symbol("2"), Symbol("•2"), :C]
  @test ddp7[:incl] == [2]

  # Vars can only be of certain types.
  DiffusionExprBody8 =  quote
    (C)::Foo
    ϕ ==  2*d₀(C)
    Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
    Ċ == ∂ₜ(C)
  end
  diffExpr8 = parse_decapode(DiffusionExprBody8)
  @test_throws ErrorException SummationDecapode(diffExpr8)

  # Multiple equality is not an accepted input
  ParseTest1 = quote
    (A, B, X)::Form0{X}
    A == d(B) == f(X)
  end
  @test_throws ErrorException parse_decapode(ParseTest1)

  # Just noting that the first decapode is denotes a X as an op1
  # while the second is a multiplication between X and F
  ParseTest2_1 = quote
    (A, B, X)::Form0{X}
    A == X(F)
  end
  pt2_1 = SummationDecapode(parse_decapode(ParseTest2_1))
  ParseTest2_2 = quote
    (A, B, X)::Form0{X}
    A == (X)F
  end
  pt2_2 = SummationDecapode(parse_decapode(ParseTest2_2))
  @test pt2_1 != pt2_2

  # Chained Tvars test
  # TODO: Do we want explict support for higher order Tvars?
  ParseTest3 = quote
    D == ∂ₜ(C)
    E == ∂ₜ(D)
  end
  pt3 = SummationDecapode(parse_decapode(ParseTest3))
  @test pt3[:name] == [:D, :E, :C]
  @test pt3[:incl] == [1,2]
  @test pt3[:src] == [3, 1]
  @test pt3[:tgt] == [1, 2]

  dot_rename!(pt3)
  @test pt3[:name] == [Symbol('C'*'\U0307'), Symbol('C'*'\U0307'*'\U0307'), :C]

  # TODO: We should eventually recognize this equivalence
  #= ParseTest4 = quote
    D == D + C
    D + C == C
  end
  pt4 = SummationDecapode(parse_decapode(ParseTest4)) =#

  # Do not rename TVars if they are given a name.
  pt5 = SummationDecapode(parse_decapode(quote
    X::Form0{Point}
    V::Form0{Point}

    k::Constant{Point}

    ∂ₜ(X) == V
    ∂ₜ(V) == -1*k*(X)
  end))

  @test pt5[:name] == [:X, :V, :k, :mult_1, Symbol('V'*'\U0307'), Symbol("-1")]
  dot_rename!(pt5)
  @test pt5[:name] == [:X, Symbol('X'*'\U0307'), :k, :mult_1, Symbol('X'*'\U0307'*'\U0307'), Symbol("-1")]

end
Deca = quote
  (A, B, C)::Form0
end

@testset "Term Construction" begin
  @test term(:(Ċ)) == Var(:Ċ)
  @test_throws ErrorException term(:(∂ₜ{Form0}))
  # @test term(Expr(:ϕ)) == Var(:ϕ)
  @test typeof(term(:(d₀(C)))) == App1
  @test typeof(term(:(∘(k, d₀)(C)))) == AppCirc1
  # @test typeof(term(:(∘(k, d₀)(C,Φ)))) == AppCirc2
  # @test term(:(∘(k, d₀)(C))) == AppCirc1([:k, :d₀], Var(:C)) #(:App1, ((:Circ, :k, :d₀), Var(:C)))
  # @test term(:(∘(k, d₀{X})(C))) == (:App1, ((:Circ, :k, :(d₀{X})), Var(:C)))
  @test_throws MethodError term(:(Ċ == ∘(⋆₀⁻¹{X}, dual_d₁{X}, ⋆₁{X})(ϕ)))
  @test term(:(∂ₜ(C))) == Tan(Var(:C))
  # @test term(:(∂ₜ{Form0}(C))) == App1(:Tan, Var(:C))
end

@testset "Recursive Expr" begin
  Recursion = quote
    x::Form0{X}
    y::Form0{X}
    z::Form0{X}

    ∂ₜ(z) == f1(x) + ∘(g, h)(y)
    y == F(f2(x), ρ(x,z))
  end

  recExpr = parse_decapode(Recursion)
  rdp = SummationDecapode(recExpr)

  @test nparts(rdp, :Var) == 8
  @test nparts(rdp, :TVar) == 1
  @test nparts(rdp, :Op1) == 4
  @test nparts(rdp, :Op2) == 2
  @test nparts(rdp, :Σ) == 1
end

@testset "Diffusion Diagram" begin
  DiffusionExprBody =  quote
      (C, Ċ)::Form0{X}
      ϕ::Form1{X}

      # Fick's first law
      ϕ ==  ∘(k, d₀)(C)
      # Diffusion equation
      Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
      ∂ₜ(C) == Ċ
  end

  diffExpr = parse_decapode(DiffusionExprBody)
  ddp = SummationDecapode(diffExpr)
  to_graphviz(ddp)

  @test nparts(ddp, :Var) == 3
  @test nparts(ddp, :TVar) == 1
  @test nparts(ddp, :Op1) == 3
  @test nparts(ddp, :Op2) == 0
end


@testset "Advection Diagram" begin
  Advection = quote
      C::Form0{X}
      (V, ϕ)::Form1{X}

      ϕ == ∧₀₁(C,V)
  end

  advdecexpr = parse_decapode(Advection)
  advdp = SummationDecapode(advdecexpr)
  @test nparts(advdp, :Var) == 3
  @test nparts(advdp, :TVar) == 0
  @test nparts(advdp, :Op1) == 0
  @test nparts(advdp, :Op2) == 1
end

@testset "Superposition Diagram" begin
  Superposition = quote
      (C, Ċ)::Form0{X}
      (ϕ, ϕ₁, ϕ₂)::Form1{X}

      ϕ == ϕ₁ + ϕ₂
      Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
      ∂ₜ(C) == Ċ
  end

  superexp = parse_decapode(Superposition)
  supdp = SummationDecapode(superexp)
  @test nparts(supdp, :Var) == 5
  @test nparts(supdp, :TVar) == 1
  @test nparts(supdp, :Op1) == 2
  @test nparts(supdp, :Op2) == 0
  @test nparts(supdp, :Σ) == 1
  @test nparts(supdp, :Summand) == 2
end

@testset "AdvectionDiffusion Diagram" begin
  AdvDiff = quote
      (C, Ċ)::Form0{X}
      (V, ϕ, ϕ₁, ϕ₂)::Form1{X}

      # Fick's first law
      ϕ₁ ==  (k ∘ d₀)(C)
      # Diffusion equation
      Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
      ϕ₂ == ∧₀₁(C,V)

      ϕ == ϕ₁ + ϕ₂
      Ċ == ∘(⋆₀⁻¹, dual_d₁, ⋆₁)(ϕ)
      ∂ₜ(C) == Ċ
  end

  advdiff = parse_decapode(AdvDiff)
  advdiffdp = SummationDecapode(advdiff)
  @test nparts(advdiffdp, :Var) == 6
  @test nparts(advdiffdp, :TVar) == 1
  @test nparts(advdiffdp, :Op1) == 4
  @test nparts(advdiffdp, :Op2) == 1
  @test nparts(advdiffdp, :Σ) == 1
  @test nparts(advdiffdp, :Summand) == 2
end

@testset "Variable Interpolation" begin
  oscillator = @decapode begin
   (X,V)::Form0
   k::Constant
   ∂ₜ(X) == V
   ∂ₜ(V) == 5.0 * X
  end
  
  k = 5.0
  oscillator_interpolated = @decapode begin
   (X,V)::Form0
   k::Constant
   ∂ₜ(X) == V
   ∂ₜ(V) == $k * X
  end
  
  @test oscillator == oscillator_interpolated
end

@testset "State Variable Inference" begin
  oscillator = @decapode begin
   (X,V)::Form0
   k::Constant
   ∂ₜ(X) == V
   ∂ₜ(V) == -k*(X)
  end
  @test issetequal([:V,:X,:k], infer_state_names(oscillator))
end

import DiagrammaticEquations: ALL_TYPES, FORM_TYPES, PRIMALFORM_TYPES,
  DUALFORM_TYPES, VECTORFIELD_TYPES, NON_EC_TYPES, USER_TYPES, NUMBER_TYPES,
  INFER_TYPES, NONINFERABLE_TYPES

@testset "Type Retrival" begin

  type_groups = [ALL_TYPES, FORM_TYPES, PRIMALFORM_TYPES, DUALFORM_TYPES,
    NON_EC_TYPES, USER_TYPES, NUMBER_TYPES, INFER_TYPES, NONINFERABLE_TYPES]


  # No repeated types
  for type_group in type_groups
    @test allunique(type_group)
  end

  equal_types(types_1, types_2) = issetequal(Set(types_1), Set(types_2))
  no_overlaps(types_1, types_2) = isempty(intersect(types_1, types_2))

  # Collections of these types should be the same
  @test equal_types(ALL_TYPES, FORM_TYPES ∪ VECTORFIELD_TYPES ∪ NON_EC_TYPES)
  @test equal_types(FORM_TYPES, PRIMALFORM_TYPES ∪ DUALFORM_TYPES)
  @test equal_types(NONINFERABLE_TYPES, USER_TYPES ∪ NUMBER_TYPES)

  # Proper seperation of types
  @test no_overlaps(FORM_TYPES ∪ VECTORFIELD_TYPES, NON_EC_TYPES)
  @test no_overlaps(PRIMALFORM_TYPES, DUALFORM_TYPES)
  @test no_overlaps(NONINFERABLE_TYPES, FORM_TYPES)
  @test INFER_TYPES == [:infer]

  @test no_overlaps(FORM_TYPES, NUMBER_TYPES)
  @test no_overlaps(FORM_TYPES, USER_TYPES)
  @test no_overlaps(USER_TYPES, NUMBER_TYPES)
end

import DiagrammaticEquations: get_unsupportedtypes
@testset "Type Validation" begin
  @test isempty(get_unsupportedtypes(ALL_TYPES))
  @test [:A] == get_unsupportedtypes([:A])
  @test [:A] == get_unsupportedtypes([:Form1, :A])
  @test isempty(get_unsupportedtypes(Symbol[]))
end

import DiagrammaticEquations: safe_modifytype

@testset "Safe Type Modification" begin
  all_types = [:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2, :Literal, :Constant, :Parameter, :PVF, :DVF, :infer]
  bad_sources = [:Literal, :Constant, :Parameter]
  good_sources = [:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2, :PVF, :DVF, :infer]

  for tgt in all_types
    for src in bad_sources
      mod, type = safe_modifytype(tgt, src)
      @test mod == false
      @test type == tgt
    end

    for src in good_sources
      mod, type = safe_modifytype(tgt, src)
      if tgt == :infer
        @test mod == true
        @test type == src
      else
        @test mod == false
        @test type == tgt
      end
    end

  end
end

@testset "Form Type Retrieval" begin
  all_types = [:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2, :Literal, :Constant, :Parameter, :PVF, :DVF, :infer]
  @test filterfor_ec_types(all_types) == [:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2, :PVF, :DVF]
  @test isempty(filterfor_ec_types(Symbol[]))
  @test isempty(filterfor_ec_types([:Literal, :Constant, :Parameter, :infer]))
end

# Warning, this testing depends on the fact that varname, form information is
# unique within a decapode even though this is not enforced
function get_name_type_pair(d::SummationDecapode)
  Set(zip(d[:name], d[:type]))
end

function test_nametype_equality(d::SummationDecapode, names_types_expected)
  @test issetequal(get_name_type_pair(d), names_types_expected)
end

@testset "Ruleset ambiguity" begin
  amb_forward_rules = [Rule(:Form0, [:Form0], :test), Rule(:Form1, [:Form0], :test)]
  @test !check_rule_ambiguity(amb_forward_rules)

  amb_back_rules = [Rule(:Form1, [:Form0], :test), Rule(:Form1, [:Form1], :test)]
  @test !check_rule_ambiguity(amb_back_rules)

  amb_large_rules = [Rule(:Form0, [:Form0, :Form1, :Form2], :test), Rule(:Form1, [:Form0, :Form1, :Form2], :test)]
  @test !check_rule_ambiguity(amb_large_rules)

  amb_large_back_rules = [Rule(:Form0, [:Form0, :Form1, :Form1], :test), Rule(:Form0, [:Form0, :Form1, :Form2], :test)]
  @test !check_rule_ambiguity(amb_large_back_rules)

  usertype_amb_rules = [Rule(:Form0, [:Constant], :test), Rule(:Form1, [:Constant], :test)]
  @test !check_rule_ambiguity(usertype_amb_rules)

  usertype_good_rules = [Rule(:Form0, [:Constant], :test), Rule(:Form0, [:Parameter], :test)]
  @test check_rule_ambiguity(usertype_good_rules)

  usertype_large_rules = [Rule(:Form0, [:Form0, :Constant], :test), Rule(:Form0, [:Form0, :Literal], :test)]
  @test check_rule_ambiguity(usertype_large_rules)

  different_rules = [Rule(:Form0, [:Form0], :test1), Rule(:Form1, [:Form0], :test2)]
  @test check_rule_ambiguity(different_rules)

  aliases_amb = [Rule(:Form0, [:Form0], :test1, [:test]), Rule(:Form1, [:Form0], :test2, [:test])]
  @test !check_rule_ambiguity(aliases_amb)

  diff_size_rules = [Rule(:Form0, [:Form0], :test), Rule(:Form1, [:Form0, :Form0], :test)]
  @test check_rule_ambiguity(diff_size_rules)

  same_rules = [Rule(:Form1, [:Form0], :test), Rule(:Form1, [:Form0], :test)]
  @test check_rule_ambiguity(same_rules)

  @test check_rule_ambiguity(default_operators(1))
  @test check_rule_ambiguity(default_operators(2))
end

@testset "Type Inference" begin

  # The type of the tgt of ∂ₜ is inferred.
  Test1 = quote
    C::Form0{X}
    ∂ₜ(C) == C
  end
  t1 = SummationDecapode(parse_decapode(Test1))
  infer_types!(t1)

  # We use set equality because we do not care about the order of the Var table.
  names_types_expected_1 = Set([(:C, :Form0)])
  test_nametype_equality(t1, names_types_expected_1)

  # The type of the src of ∂ₜ is inferred.
  Test2 = quote
    C::infer{X}
    ∂ₜ(C) == C
  end
  t2 = SummationDecapode(parse_decapode(Test2))
  t2[only(incident(t2, :C, :name)), :type] = :Form0
  infer_types!(t2)

  names_types_expected_2 = Set([(:C, :Form0)])
  test_nametype_equality(t2, names_types_expected_2)

  # The type of the tgt of d is inferred.
  Test3 = quote
    C::Form0{X}
    D::infer{X}
    E::infer{X}
    #C::infer{X}
    #D::Form1{X}
    #E::infer{X}
    D == d(C)
    E == d(D)
  end
  t3 = SummationDecapode(parse_decapode(Test3))
  #t3_inferred = infer_types!(t3)
  infer_types!(t3)

  names_types_expected_3 = Set([(:C, :Form0), (:D, :Form1), (:E, :Form2)])
  test_nametype_equality(t3, names_types_expected_3)

  # The type of the src and tgt of d is inferred.
  Test4 = quote
    C::infer{X}
    D::Form1{X}
    E::infer{X}
    D == d(C)
    E == d(D)
  end
  t4 = SummationDecapode(parse_decapode(Test4))
  #t4_inferred = infer_types!(t4)
  infer_types!(t4)

  names_types_expected_4 = Set([(:C, :Form0), (:D, :Form1), (:E, :Form2)])
  test_nametype_equality(t4, names_types_expected_4)

  # The type of the src of d is inferred.
  Test5 = quote
    C::infer{X}
    D::Form1{X}
    D == d(C)
  end
  t5 = SummationDecapode(parse_decapode(Test5))
  infer_types!(t5)

  names_types_expected_5 = Set([(:C, :Form0), (:D, :Form1)])
  test_nametype_equality(t5, names_types_expected_5)

  # The type of the src of d is inferred.
  Test6 = quote
    A::Form0{X}
    (B,C,D,E,F)::infer{X}
    B == d(A)
    C == d(B)
    D == ⋆(C)
    E == d(D)
    F == d(E)
  end
  t6 = SummationDecapode(parse_decapode(Test6))
  infer_types!(t6)

  names_types_expected_6 = Set([
    (:A, :Form0),     (:B, :Form1),     (:C, :Form2),
    (:F, :DualForm2), (:E, :DualForm1), (:D, :DualForm0)])
  test_nametype_equality(t6, names_types_expected_6)

  # The type of a summand is inferred.
  Test7 = quote
    A::Form0{X}
    (B,C,D,E,F)::infer{X}
    A == B+C+D+E+F
  end
  t7 = SummationDecapode(parse_decapode(Test7))
  infer_types!(t7)

  types_7 = Set(t7[:type])
  types_expected_7 = Set([:Form0])
  @test issetequal(types_7, types_expected_7)

  # The type of a sum is inferred.
  Test8 = quote
    B::Form0{X}
    (A,C,D,E,F)::infer{X}
    A == B+C+D+E+F
  end
  t8 = SummationDecapode(parse_decapode(Test8))
  infer_types!(t8)

  types_8 = Set(t8[:type])
  types_expected_8 = Set([:Form0])
  @test issetequal(types_8, types_expected_8)

  # Type inference of op1 propagates through sums.
  Test8 = quote
    Γ::Form0{X}
    (A,B,C,D,E,F,Θ)::infer{X}
    B == d(Γ)
    A == B+C+D+E+F
    Θ == d(A)
  end
  t8 = SummationDecapode(parse_decapode(Test8))
  infer_types!(t8)

  names_types_expected_8 = Set([
    (:Γ, :Form0),
    (:A, :Form1), (:B, :Form1), (:C, :Form1), (:D, :Form1), (:E, :Form1), (:F, :Form1),
    (:Θ, :Form2)])
  test_nametype_equality(t8, names_types_expected_8)

  function makeInferPathDeca(log_cycles; infer_path = false)
    cycles = 2 ^ log_cycles
    num_nodes = 6 * cycles
    num_ops = num_nodes - 1

    if(infer_path)
      type_arr = vcat(:Form0, fill(:infer, num_nodes - 1))
    else
      type_arr = repeat([:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2], cycles)
    end

    InferPathTest = @acset SummationDecapode{Any, Any, Symbol} begin
      Var = num_nodes
      type = type_arr
      name = map(x -> Symbol("•$x"), 1:num_nodes)

      Op1 = num_ops
      src = 1:num_ops
      tgt = 2:num_ops + 1
      op1 = repeat([:d, :d, :⋆, :d, :d, :⋆], cycles)[1:num_ops]
    end
  end
  # log_cycles > 6 starts to be slow
  log_cycles = 3

  decapode_to_infer = makeInferPathDeca(log_cycles, infer_path = true)
  decapode_expected = makeInferPathDeca(log_cycles)
  infer_types!(decapode_to_infer)
  @test decapode_to_infer == decapode_expected

  # Inference can happen "through" composed ops.
  Test9 = quote
    A::Form0{X}
    B::infer{X}
    B == ∘(d,d,⋆,d,d)(A)
  end
  t9 = SummationDecapode(parse_decapode(Test9))
  t9 = expand_operators(t9)
  infer_types!(t9)

  names_types_expected_9 = Set([
    (:A, :Form0),     (Symbol("•_1_", 1), :Form1),     (Symbol("•_1_", 2), :Form2),
    (:B, :DualForm2), (Symbol("•_1_", 4), :DualForm1), (Symbol("•_1_", 3), :DualForm0)])
  test_nametype_equality(t9, names_types_expected_9)

  # Basic op2 inference using ∧
  Test10 = quote
    (A, B)::Form0{X}
    C::infer{X}

    D::Form0{X}
    E::Form1{X}
    (F, H)::infer{X}

    C == ∧(A, B)

    F == ∧(D, E)
    H == ∧(E, D)
  end
  t10 = SummationDecapode(parse_decapode(Test10))
  infer_types!(t10)

  names_types_expected_10 = Set([(:F, :Form1), (:B, :Form0), (:C, :Form0), (:H, :Form1), (:A, :Form0), (:E, :Form1), (:D, :Form0)])
  test_nametype_equality(t10, names_types_expected_10)

  # Basic op2 inference using L
  Test11 = quote
    A::Form1{X}
    E::DualForm1{X}
    B::DualForm0{X}
    (C, D)::infer{X}

    C == L(A, B)
    D == L(A, E)
  end
  t11 = SummationDecapode(parse_decapode(Test11))
  infer_types!(t11)


  names_types_expected_11 = Set([(:A, :Form1), (:B, :DualForm0), (:C, :DualForm0), (:E, :DualForm1), (:D, :DualForm1)])
  test_nametype_equality(t11, names_types_expected_11)

  # Basic op2 inference using i
  Test12 = quote
    A::Form1{X}
    B::DualForm1{X}
    C::infer{X}

    C == i(A, B)
  end
  t12 = SummationDecapode(parse_decapode(Test12))
  infer_types!(t12)

  names_types_expected_12 = Set([(:A, :Form1), (:B, :DualForm1), (:C, :DualForm0)])
  test_nametype_equality(t12, names_types_expected_12)

  #2D op2 inference using ∧
  Test13 = quote
    (A, B)::Form1{X}
    C::infer{X}

    D::Form0{X}
    E::Form2{X}
    (F, H)::infer{X}

    C == ∧(A, B)

    F == ∧(D, E)
    H == ∧(E, D)
  end
  t13 = SummationDecapode(parse_decapode(Test13))
  infer_types!(t13)

  names_types_expected_13 = Set([(:E, :Form2), (:B, :Form1), (:C, :Form2), (:A, :Form1), (:F, :Form2), (:H, :Form2), (:D, :Form0)])
  test_nametype_equality(t13, names_types_expected_13)

  # 2D op2 inference using L
  Test14 = quote
    A::Form1{X}
    B::DualForm2{X}
    C::infer{X}

    C == L(A, B)
  end
  t14 = SummationDecapode(parse_decapode(Test14))
  infer_types!(t14)

  names_types_expected_14 = Set([(:C, :DualForm2), (:A, :Form1), (:B, :DualForm2)])
  test_nametype_equality(t14, names_types_expected_14)

  # 2D op2 inference using i
  Test15 = quote
    A::Form1{X}
    B::DualForm2{X}
    C::infer{X}

    C == i(A, B)
  end
  t15 = SummationDecapode(parse_decapode(Test15))
  infer_types!(t15)

  names_types_expected_15 = Set([(:A, :Form1), (:B, :DualForm2), (:C, :DualForm1)])
  test_nametype_equality(t15, names_types_expected_15)

  # Special case of a summation with a mix of infer and Literal.
  t16 = @decapode begin
    A == 2 + C + D
  end
  infer_types!(t16)

  names_types_expected_16 = Set([(:A, :infer), (:C, :infer), (:D, :infer), (Symbol("2"), :Literal)])
  test_nametype_equality(t16, names_types_expected_16)

  # Special case of a summation with a mix of Form, Constant, and infer.
  t17 = @decapode begin
    A::Form0
    C::Constant
    A == C + D
  end
  infer_types!(t17)

  names_types_expected_17 = Set([(:A, :Form0), (:C, :Constant), (:D, :Form0)])
  test_nametype_equality(t17, names_types_expected_17)

  # Special case of a summation with a mix of infer and Constant.
  t18 = @decapode begin
    C::Constant
    A == C + D
  end
  infer_types!(t18)

  names_types_expected_18 = Set([(:A, :infer), (:C, :Constant), (:D, :infer)])
  test_nametype_equality(t18, names_types_expected_18)

  # Test #19: Prevent intermediates from converting to Constant
  let
    d = @decapode begin
      h::Form0
      Γ::Form1
      n::Constant

      ḣ == ∂ₜ(h)
      ḣ == ∘(⋆, d, ⋆)(Γ * d(h) * avg₀₁(mag(♯(d(h)))^(n-1)) * avg₀₁(h^(n+2)))
    end

    infer_types!(d, dim = 1)
    @test d[18, :type] != :Constant
  end

  # Test #20: Prevent intermediates from converting to Literal
  let
    d = @decapode begin
      h::Form0
      Γ::Form1
      n::Constant

      ḣ == ∂ₜ(h)
      ḣ == ∘(⋆, d, ⋆)(Γ * d(h) * avg₀₁(mag(♯(d(h)))^(n-1)) * avg₀₁(h^(n+2)))
    end

    d = expand_operators(d)
    infer_types!(d, dim = 1)

    @test d[8, :type] != :Literal
  end

  # Test #21: Prevent intermediates from converting to Parameter
  let
    d = @decapode begin
      C::Parameter
      A == C + D
    end
    infer_types!(d)

    names_types_expected = Set([(:A, :infer), (:C, :Parameter), (:D, :infer)])
    test_nametype_equality(d, names_types_expected)
  end

  # Test #22: Have source form information flow over Parameters and Constants
  let
    d = @decapode begin
      B::Form0
      C::Parameter
      D::Constant
      A == C + D + B
    end
    infer_types!(d)

    names_types_expected = Set([(:A, :Form0), (:B, :Form0), (:C, :Parameter), (:D, :Constant)])
    test_nametype_equality(d, names_types_expected)
  end

  # Test #23: Have target form information flow over Parameters and Constants
  let
    d = @decapode begin
      A::Form0
      C::Parameter
      D::Constant
      A == C + D + B
    end
    infer_types!(d)

    names_types_expected = Set([(:A, :Form0), (:B, :Form0), (:C, :Parameter), (:D, :Constant)])
    test_nametype_equality(d, names_types_expected)
  end

  # Test #24: Summing mismatched forms throws an error
  let
    d = @decapode begin
      B::Form0
      C::Form1
      A == C + D + B
    end
    @test_throws "Type mismatch in summation" infer_types!(d)
  end

  # Test #25: Infer between flattened and sharpened vector fields.
  let
    d = @decapode begin
      A::Form1
      B::DualForm1
      C::PVF
      D::DVF

      A == ♭(E)
      B == ♭(F)
      C == ♯(G)
      D == ♯(H)

      I::Form1
      J::DualForm1
      K::PVF
      L::DVF

      M == ♯(I)
      N == ♯(J)
      O == ♭(K)
      P == ♭(L)
    end
    infer_types!(d)

    # TODO: Update this as more sharps and flats are released.
    names_types_expected = Set([(:A, :Form1), (:B, :DualForm1), (:C, :PVF), (:D, :DVF),
                               (:E, :DVF), (:F, :infer), (:G, :Form1), (:H, :DualForm1),
                               (:I, :Form1), (:J, :DualForm1), (:K, :PVF), (:L, :DVF),
                               (:M, :PVF), (:N, :DVF), (:O, :infer), (:P, :Form1)])
    test_nametype_equality(d, names_types_expected)
  end

  op_not_in_rules = @decapode begin
    A::Form0
    C::Form0

    B == fake(A)
    D == fake2(A, C)
  end
  op_not_in_rules_res = deepcopy(op_not_in_rules)
  infer_types!(op_not_in_rules)

  @test op_not_in_rules == op_not_in_rules_res
  @test !(op_not_in_rules === op_not_in_rules_res)

  only_type_dec = @decapode begin
    A::Form0

    B == fake(A)
    C == d(A)
  end
  infer_types!(only_type_dec)

  only_type_dec_res = Set([(:A, :Form0), (:B, :infer), (:C, :Form1)])
  test_nametype_equality(only_type_dec, only_type_dec_res)
end

@testset "Overloading Resolution and Type Checking" begin
  # d overloading is resolved.
  t1 = @decapode begin
    A::Form0{X}
    B::Form1{X}
    C::Form2{X}
    D::DualForm0{X}
    E::DualForm1{X}
    F::DualForm2{X}
    B == d(A)
    C == d(B)
    E == d(D)
    F == d(E)
  end
  resolve_overloads!(t1)

  op1s_1 = t1[:op1]
  op1s_expected_1 = [:d₀ , :d₁, :dual_d₀, :dual_d₁]
  @test op1s_1 == op1s_expected_1
  @test type_check(t1)

  # ⋆ overloading is resolved.
  t2 = @decapode begin
    C::Form0{X}
    D::DualForm2{X}
    E::Form1{X}
    F::DualForm1{X}
    G::Form2{X}
    H::DualForm0{X}
    D == ⋆(C)
    C == ⋆(D)
    E == ⋆(F)
    F == ⋆(E)
    G == ⋆(H)
    H == ⋆(G)
  end
  resolve_overloads!(t2)
  @test type_check(t2)

  op1s_2 = t2[:op1]
  # Note: The Op1 table of the decapode created does not have the functions
  # listed in the order in which they appear in Test2.
  op1s_expected_2 = [:⋆₀ , :⋆₀⁻¹ , :⋆₁⁻¹ , :⋆₁ , :⋆₂⁻¹ , :⋆₂]
  @test op1s_2 == op1s_expected_2

  # All overloading on the de Rahm complex is resolved.
  t3 = @decapode begin
    A::Form0{X}
    B::Form1{X}
    C::Form2{X}
    D::DualForm0{X}
    E::DualForm1{X}
    F::DualForm2{X}
    B == d(A)
    C == d(B)
    E == d(D)
    F == d(E)
    F == ⋆(A)
    A == ⋆(F)
    E == ⋆(B)
    B == ⋆(E)
    D == ⋆(C)
    C == ⋆(D)
  end
  resolve_overloads!(t3)

  op1s_3 = t3[:op1]
  # Note: The Op1 table of the decapode created does not have the functions
  # listed in the order in which they appear in Test2.
  op1s_expected_3 = [:d₀ , :d₁, :dual_d₀, :dual_d₁, :⋆₀ , :⋆₀⁻¹ , :⋆₁ , :⋆₁⁻¹ , :⋆₂ , :⋆₂⁻¹]
  @test op1s_3 == op1s_expected_3
  @test type_check(t3)

  t4 = @decapode begin
    (A, C)::Form0{X}
    (B, D, E)::Form1{X}

    (I,H,F)::DualForm0{X}
    (J,G)::DualForm1{X}

    C == ∧(A, A)
    D == ∧(A, B)
    E == ∧(B, A)
    F == L(B, I)
    G == L(B, J)
    H == i(B, J)
  end
  resolve_overloads!(t4)
  op2s_4 = t4[:op2]
  op2s_expected_4 = [:∧₀₀ , :∧₀₁, :∧₁₀, :L₀, :L₁, :i₁]
  @test op2s_4 == op2s_expected_4
  @test type_check(t4)

  t5 = @decapode begin
    A::Form0{X}
    B::Form1{X}
    (C, D, E, F)::Form2{X}

    (I, G)::DualForm2{X}
    H::DualForm1{X}

    D == ∧(B, B)
    E == ∧(A, C)
    F == ∧(C, A)
    G == L(B, I)
    H == i(B, I)
  end
  resolve_overloads!(t5)
  op2s_5 = t5[:op2]
  op2s_expected_5 = [:∧₁₁, :∧₀₂, :∧₂₀, :L₂, :i₂]
  @test op2s_5 == op2s_expected_5
  @test type_check(t5)

  t6 = @decapode begin
    A::Form1
    B::Form1

    Y == ♯ᵖᵈ(A)
    Z == ♯(B)
  end
  resolve_overloads!(infer_types!(t6))
  op1s_6 = t6[:op1]
  op1s_expected_6 = [:♯ᵖᵈ, :♯ᵖᵖ]
  @test op1s_6 == op1s_expected_6
  @test type_check(t6)

  op_not_in_rules = @decapode begin
    A::Form0
    B::Form0
    C::Form0
    D::Form0

    B == fake(A)
    D == fake2(A, C)
  end
  op_not_in_rules_res = deepcopy(op_not_in_rules)
  resolve_overloads!(op_not_in_rules)

  @test op_not_in_rules == op_not_in_rules_res
  @test op_not_in_rules !== op_not_in_rules_res
  @test type_check(op_not_in_rules)

  only_type_dec = @decapode begin
    A::Form0
    B::Form0
    C::Form1

    B == fake(A)
    C == d(A)
  end
  resolve_overloads!(only_type_dec)

  only_type_dec_res = Set([(:A, :Form0), (:B, :Form0), (:C, :Form1)])
  test_nametype_equality(only_type_dec, only_type_dec_res)
  @test type_check(only_type_dec)

  poorly_type_deca = @decapode begin
    (A,B)::Form0

    B == d(A)
  end
  resolve_overloads!(poorly_type_deca)
  @test_throws DecaTypeExeception type_check(poorly_type_deca)
end

@testset "Type Inference and Overloading Resolution Integration" begin

  start_just_op1s = @decapode begin
    B::Form0
    A == d(d(B))
  end

  infer_resolve!(start_just_op1s)
  test_nametype_equality(start_just_op1s, Set([(Symbol("•1"), :Form1), (:B, :Form0), (:A, :Form2)]))
  @test start_just_op1s[:op1] == [:d₀, :d₁]

  end_just_op1s = @decapode begin
    A::Form2
    A == d(d(B))
  end
  infer_resolve!(end_just_op1s)
  test_nametype_equality(end_just_op1s, Set([(Symbol("•1"), :Form1), (:B, :Form0), (:A, :Form2)]))
  @test end_just_op1s[:op1] == [:d₀, :d₁]

  all_op_types = @decapode begin
    (I1, I2)::Form0
    A == Δ(d(∧(I1, I2))) + δ(I3)
  end
  infer_resolve!(all_op_types)
  @test all_op_types[3, :name] == :A && all_op_types[3, :type] == :Form1
  @test all_op_types[8, :name] == :I3 && all_op_types[8, :type] == :Form2

  @test all_op_types[:op1] == [:d₀, :Δ₁, :δ₂]
  @test all_op_types[:op2] == [:∧₀₀]

  arithmetic = @decapode begin
    B::Form0
    A == (n+1) .* B
  end
  infer_resolve!(arithmetic)

  @test_broken arithmetic[2, :name] == :A && arithmetic[2, :type] == :Form0

  # Infer types and resolve overloads for the Halfar equation.
  # TODO: This test isn't passing right now because we can't ignore type of the power
  # in the exponent.
  let
    d = @decapode begin
      h::Form0
      Γ::Form1
      n::Constant

      ∂ₜ(h) == ∘(⋆, d, ⋆)(Γ  * d(h) ∧ (mag(♯(d(h)))^(n-1)) ∧ (h^(n+2)))
    end
    d = expand_operators(d)
    infer_resolve!(d)
    @test_broken d == @acset SummationDecapode{Any, Any, Symbol} begin
      Var = 19
      TVar = 1
      Op1 = 8
      Op2 = 6
      Σ = 1
      Summand = 2
      src = [1, 1, 1, 13, 12, 6, 18, 19]
      tgt = [4, 9, 13, 12, 11, 18, 19, 4]
      proj1 = [2, 3, 11, 8, 1, 7]
      proj2 = [9, 15, 14, 10, 5, 16]
      res = [8, 14, 10, 7, 16, 6]
      incl = [4]
      summand = [3, 17]
      summation = [1, 1]
      sum = [5]
      op1 = [:∂ₜ, :d₀, :d₀, :♯ᵖᵖ, :norm, :⋆₁, :dual_d₁, :⋆₀⁻¹]
      op2 = [:*, :-, :^, :∧₁₀, :^, :∧₁₀]
      type = [:Form0, :Form1, :Constant, :Form0, :infer, :Form1, :Form1, :Form1, :Form1, :Form0, :Form0, :PVF, :Form1, :infer, :Literal, :Form0, :Literal, :DualForm1, :DualForm2]
      name = [:h, :Γ, :n, :ḣ, :sum_1, Symbol("•2"), Symbol("•3"), Symbol("•4"), Symbol("•5"), Symbol("•6"), Symbol("•7"), Symbol("•8"), Symbol("•9"), Symbol("•10"), Symbol("1"), Symbol("•11"), Symbol("2"), Symbol("•_6_1"), Symbol("•_6_2")]
    end
  end

  # Momentum-formulation of Navier Stokes on sphere
  DiffusionExprBody = quote
    (T, Ṫ)::Form0{X}
    ϕ::DualForm1{X}
    k::Parameter{X}
    # Fick's first law
    ϕ ==  ⋆(k*d(T))
    # Diffusion equation
    Ṫ ==  ⋆(d(ϕ))
  end
  Diffusion = SummationDecapode(parse_decapode(DiffusionExprBody))
  AdvectionExprBody = quote
    (M,V)::Form1{X}  #  M = ρV
    (ρ, p, T, Ṫ)::Form0{X}
    V == M/avg(ρ)
    ρ == p / R₀(T)
    Ṫ == neg(⋆(L(V, ⋆(T))))
  end
  Advection = SummationDecapode(parse_decapode(AdvectionExprBody))
  SuperpositionExprBody = quote
    (T, Ṫ, Ṫ₁, Ṫₐ)::Form0{X}
    Ṫ == Ṫ₁ + Ṫₐ
    ∂ₜ(T) == Ṫ
  end
  Superposition = SummationDecapode(parse_decapode(SuperpositionExprBody))
  compose_continuity = @relation () begin
    diffusion(T, Ṫ₁)
    advection(M, ρ, P, T, Ṫₐ)
    superposition(T, Ṫ, Ṫ₁, Ṫₐ)
  end
  continuity_cospan = oapply(compose_continuity,
                  [Open(Diffusion, [:T, :Ṫ]),
                  Open(Advection, [:M, :ρ, :p, :T, :Ṫ]),
                  Open(Superposition, [:T, :Ṫ, :Ṫ₁, :Ṫₐ])])

  continuity = apex(continuity_cospan)
  NavierStokesExprBody = quote
    (M, Ṁ, G, V)::Form1{X}
    (T, ρ, p, ṗ)::Form0{X}
    (two,three,kᵥ)::Parameter{X}
    V == M/avg(ρ)
    Ṁ == neg(L(V, ⋆(V)))*avg(ρ) +
          kᵥ*(Δ(V) + d(δ(V))/three) +
          d(i(V, ⋆(V))/two)*avg(ρ) +
          neg(d(p)) +
          G*avg(ρ)
    ∂ₜ(M) == Ṁ
    ṗ == neg(⋆(L(V, ⋆(p)))) # *Lie(3Form) = Div(*3Form x v) --> conservation of pressure
    ∂ₜ(p) == ṗ
  end
  NavierStokes = SummationDecapode(parse_decapode(NavierStokesExprBody))
  compose_heatXfer = @relation () begin
    continuity(M, ρ, P, T)
    navierstokes(M, ρ, P, T)
  end
  heatXfer_cospan = oapply(compose_heatXfer,
                  [Open(continuity, [:M, :ρ, :P, :T]),
                  Open(NavierStokes, [:M, :ρ, :p, :T])])
  HeatXfer = apex(heatXfer_cospan)
  infer_types!(HeatXfer)
  resolve_overloads!(HeatXfer)

  @test_throws DecaTypeExeception type_check(HeatXfer)
  @test HeatXfer[12, :op2] == :*
  @test HeatXfer[40, :type] == :DualForm1 && HeatXfer[39, :type] == :Form1

  deca_3d = @decapode begin
    (P0::Form0)
    (P1::Form1)
    (P2::Form2)
    (P3::Form3)

    (D0::DualForm0)
    (D1::DualForm1)
    (D2::DualForm2)
    (D3::DualForm3)

    R1 == d(P2)
    R2 == d(D2)

    R3 == δ(P3)

    R4 == Δ(P3)

    R5 == -(P3)
    R6 == -(D3)

    R7 == ⋆(P0)
    R8 == ⋆(P1)
    R9 == ⋆(P2)
    R10 == ⋆(P3)

    R11 == ⋆(D0)
    R12 == ⋆(D1)
    R13 == ⋆(D2)
    R14 == ⋆(D3)

    R15 == ∂ₜ(P3)
    R16 == ∂ₜ(D3)

    R17 == ∧(P2, P1)
    R18 == ∧(P1, P2)

    R19 == L(P1, D3)
    R20 == i(P1, D3)

    R21 == ∧(P3, P0)
    R22 == ∧(P0, P3)

  end

  infer_types!(deca_3d, dim=3)
  @test type_check(deca_3d, dim=3)

  resolve_overloads!(deca_3d, dim=3)

  # Compare pprint of Term(deca_3d) against the expected repr copied from the
  # test environment.  This avoids fragile ACSet row-position indexing and
  # fully documents the inferred types and resolved operators in one place.
  expected_pprint = """Context:
  P0::Form0 over I
  P1::Form1 over I
  P2::Form2 over I
  P3::Form3 over I
  D0::DualForm0 over I
  D1::DualForm1 over I
  D2::DualForm2 over I
  D3::DualForm3 over I
  R1::Form3 over I
  R12::Form2 over I
  R2::DualForm3 over I
  R20::DualForm2 over I
  R3::Form2 over I
  R13::Form1 over I
  R4::Form3 over I
  R18::Form3 over I
  R5::Form3 over I
  R14::Form0 over I
  R6::DualForm3 over I
  R22::Form3 over I
  R7::DualForm3 over I
  R15::Form3 over I
  R8::DualForm2 over I
  R19::DualForm3 over I
  R9::DualForm1 over I
  R16::DualForm3 over I
  R10::DualForm0 over I
  R21::Form3 over I
  R11::Form3 over I
  R17::Form3 over I
Equations:
R1   = d₂(P2)
R2   = dual_d₂(D2)
R3   = δ₃(P3)
R4   = Δ₃(P3)
R5   = -(P3)
R6   = -(D3)
R7   = ⋆₀(P0)
R8   = ⋆₁(P1)
R9   = ⋆₂(P2)
R10   = ⋆₃(P3)
R11   = ⋆₃⁻¹(D0)
R12   = ⋆₂⁻¹(D1)
R13   = ⋆₁⁻¹(D2)
R14   = ⋆₀⁻¹(D3)
R15   = ∂ₜ(P3)
R16   = ∂ₜ(D3)
R17   = ∧₂₁(P2, P1)
R18   = ∧₁₂(P1, P2)
R19   = L₃(P1, D3)
R20   = i₃(P1, D3)
R21   = ∧₃₀(P3, P0)
R22   = ∧₀₃(P0, P3)
"""
  @test sprint((io, x) -> DiagrammaticEquations.pprint(io, x), Term(deca_3d)) == expected_pprint
end

@testset "Compilation Transformation" begin

  # contract_operators does not change the order of op1s.
  Test1 = quote
    (A,B)::Form0{X}
    B == ∘(j,i,h,g,f)(A)
  end
  t1_orig = SummationDecapode(parse_decapode(Test1))
  t1_expanded = expand_operators(t1_orig)
  t1_contracted = contract_operators(t1_expanded)
  @test t1_orig == t1_contracted
  # contract_operators does not mutate its argument.
  @test !(t1_contracted === t1_expanded)

  # contract_operators works on multiple chains.
  Test2 = quote
    (A,B,C,D)::Form0{X}
    B == ∘(d,d,⋆,d,d)(A)
    D == ∘(d,d,⋆,d,d)(C)
  end
  t2_orig = SummationDecapode(parse_decapode(Test2))
  t2_expanded = expand_operators(t2_orig)
  t2_contracted = contract_operators(t2_expanded)
  #@test t2_orig == t2_contracted
  @test issetequal(
                  [(t2_orig[:src][i], t2_orig[:tgt][i], t2_orig[:op1][i]) for i in parts(t2_orig, :Op1)],
                  [(t2_contracted[:src][i], t2_contracted[:tgt][i], t2_contracted[:op1][i]) for i in parts(t2_contracted, :Op1)])


  # contract_operators "absorbs ∘s".
  t3_orig = SummationDecapode(parse_decapode(quote
    (A,B)::Form0{X}
    B == f(∘(g)(A))
    #D == ∘(g)(f(C)) # Uncomment when parsing supports this
  end))
  t3_expanded = expand_operators(t3_orig)
  t3_contracted = contract_operators(t3_expanded)
  @test t3_contracted == SummationDecapode(parse_decapode(quote
    (A,B)::Form0{X}
    B == ∘(g,f)(A)
  end))

  # contract_operators does not contract through when a var is
  # the target of multiple ops.
  t4_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D)::Form0{X}
    C == f(A)
    C == g(B)
    D == h(C)
  end))
  @test t4_orig == contract_operators(t4_orig)

  # contract_operators does not contract through when a var is a summand.
  t5_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D,E)::Form0{X}
    B == f(A)
    C == g(B)
    E == B + D
  end))
  @test t5_orig == contract_operators(t5_orig)

  # recursive_delete_parents does not change an empty Decapode.
  t6_orig = SummationDecapode(parse_decapode(quote

  end))
  t6_rec_del = recursive_delete_parents(t6_orig, Vector{Int64}())
  @test t6_orig == SummationDecapode{Any, Any, Symbol}()
  # recursive_delete_parents does not mutate its argument.
  @test !(t6_orig === t6_rec_del)

  # recursive_delete_parents deletes a chain of single-child parents.
  t7_orig = SummationDecapode(parse_decapode(quote
    (A,B,C)::Form0{X}
    B == f(A)
    C == g(B)
  end))
  t7_rec_del = recursive_delete_parents(t7_orig, incident(t7_orig, :C, :name))
  @test t7_rec_del == SummationDecapode{Any, Any, Symbol}()

  # recursive_delete_parents deletes a chain of single-child parents until a
  # multi-child parent is found.
  t8_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D,E)::Form0{X}
    B == f(A)
    C == g(B)
    D == h(C)

    E == i(B)
  end))
  t8_rec_del = recursive_delete_parents(t8_orig, incident(t8_orig, :D, :name))
  @test t8_rec_del == SummationDecapode(parse_decapode(quote
    (A, B, E)::Form0{X}
    B == f(A)

    E == i(B)
  end))

  # recursive_delete_parents deletes a multi-child parent if all its children are
  # deleted.
  t9_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D,E)::Form0{X}
    B == f(A)
    C == g(B)

    D == i(A)
    E == j(D)
  end))
  t9_rec_del = recursive_delete_parents(t9_orig,
                                reduce(vcat, incident(t9_orig, [:C, :E], :name)))
  @test t9_rec_del == SummationDecapode{Any, Any, Symbol}()

  # recursive_delete_parents deletes π₁ and π₂ of an Op2 with missing result.
  t10_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D)::Form0{X}
    C == f(A,B)
    D == g(C)
  end))
  t10_rec_del = recursive_delete_parents(t10_orig, incident(t10_orig, :D, :name))
  @test t10_rec_del == SummationDecapode{Any, Any, Symbol}()

  # recursive_delete_parents only deletes an π₁ or π₂ of an Op2 if it is not used in
  # another Op2.
  t11_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D,E)::Form0{X}
    D == f(A,B)
    E == g(B,C)
  end))
  t11_rec_del = recursive_delete_parents(t11_orig, incident(t11_orig, :D, :name))
  # We should test if Decapodes are isomorphic, but Catlab
  # doesn't do this yet.
  #@test t11_rec_del == SummationDecapode(parse_decapode(quote
  #  (B,C,E)::Form0{X}
  #  E == g(B,C)
  #end))
  @test t11_rec_del == @acset SummationDecapode{Any, Any, Symbol} begin
    Var  = 3
    Op2  = 1

    type = [:Form0, :Form0, :Form0]
    name = [:E, :B, :C]

    proj1 = [2]
    proj2 = [3]
    res   = [1]
    op2   = [:g]
  end

  # If π₁ and π₂ of an Op2 point to the same Var, that Var is used in some
  # other computation, but this Op2 has deleted result, then delete the Op2,
  # but not the Var.
  t12_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D)::Form0{X}
    C == f(A,A)
    D == g(A,B)
  end))
  t12_rec_del = recursive_delete_parents(t12_orig, incident(t12_orig, :C, :name))
  @test t12_rec_del == SummationDecapode(parse_decapode(quote
    (A,B,D)::Form0{X}
    D == g(A,B)
  end))

  # recursive_delete_parents does not delete a Var if it is used in a summation.
  t13_orig = SummationDecapode(parse_decapode(quote
    (A,B,C,D,E)::Form0{X}
    D == A + B + C

    E == f(A)
  end))
  t13_rec_del = recursive_delete_parents(t13_orig, incident(t13_orig, :E, :name))
  @test t13_rec_del == SummationDecapode(parse_decapode(quote
    (A,B,C,D)::Form0{X}
    D == A + B + C
  end))

  # Test recursive_delete_parents on a real physics.
  Veronis = SummationDecapode(parse_decapode(quote
    B::DualForm0{X}
    E::Form1{X}
    σ::Form1{X}
    J::Form1{X}
    (negone,c,ε₀)::Constant{X}

    ∂ₜ(E) == ((negone * (J - σ .* E))./ε₀) + ((c * c).*(⋆₁⁻¹(d₀(B))))

    ∂ₜ(B) == ⋆₂(d₁(E))
  end))
  Veronis_rec_del = recursive_delete_parents(Veronis, incident(Veronis, :Ḃ, :name))
  @test Veronis_rec_del == SummationDecapode(parse_decapode(quote
    B::DualForm0{X}
    E::Form1{X}
    σ::Form1{X}
    J::Form1{X}
    (negone,c,ε₀)::Constant{X}

    ∂ₜ(E) == ((negone * (J - σ .* E))./ε₀) + ((c * c).*(⋆₁⁻¹(d₀(B))))
  end))

  # Test recursive_delete_parents on a real physics.
  Veronis = SummationDecapode(parse_decapode(quote
    B::DualForm0{X}
    E::Form1{X}
    σ::Form1{X}
    J::Form1{X}
    (negone,c,ε₀)::Constant{X}

    ∂ₜ(E) == ((negone * (J - σ .* E))./ε₀) + ((c * c).*(⋆₁⁻¹(d₀(B))))

    ∂ₜ(B) == ⋆₂(d₁(E))
  end))
  Veronis_rec_del = recursive_delete_parents(Veronis, incident(Veronis, :Ė, :name))
  # We should test if Decapodes are isomorphic, but Catlab
  # doesn't do this yet.
  #@test Veronis_rec_del == SummationDecapode(parse_decapode(quote
  #  B::DualForm0{X}
  #  E::Form1{X}

  #  ∂ₜ(B) == ⋆₂(d₁(E))
  #end))
  @test Veronis_rec_del ==  @acset SummationDecapode{Any, Any, Symbol} begin
    Var  = 4
    TVar = 1
    Op1  = 3

    type = [:DualForm0, :Form1, :infer, :infer]
    name = [:B, :E, :sum_1, :Ḃ]

    incl = [4]

    src = [3, 1, 2]
    tgt = [4, 4, 3]

    op1 = [:⋆₂, :∂ₜ, :d₁]
  end

  t14_orig = @decapode begin
    C == a(b(c(d(e(f(g(D)))))))
  end
  t14_contracted = contract_operators(t14_orig,
    black_list=Set([:d]))
  @test issetequal(t14_contracted[:op1], [:d, [:g, :f, :e], [:c, :b, :a]])

  t15_orig = @decapode begin
    C == a(b(c(d(e(f(g(D)))))))
  end
  t15_contracted = contract_operators(t15_orig,
    white_list=Set([:a, :b, :c]),
    black_list=Set([:d]))
  @test issetequal(t15_contracted[:op1], [:g, :f, :e, :d, [:c, :b, :a]])
end

@testset "ASCII & Vector Calculus Operators" begin
# Test ASCII to Unicode conversion on an Op2.
t1 = @decapode begin
  A == wedge(C, D)
end
unicode!(t1)

op2s_1 = Set(t1[:op2])
op2s_expected_1 = Set([:∧])
@test issetequal(op2s_1, op2s_expected_1)

# Test ASCII to Unicode conversion with multiple occurences of the same operator.
t2 = @decapode begin
  A == wedge(C, D)
  B == wedge(E, F)
end
unicode!(t2)

op2s_2 = Set(t2[:op2])
op2s_expected_2 = Set([:∧])
@test issetequal(op2s_2, op2s_expected_2)

# Test ASCII to Unicode conversion works with composed operators after expansion.
t3 = @decapode begin
  A == ∘(star, lapl, star_inv)(B)
end
t3 = expand_operators(t3)
unicode!(t3)

op1s_3 = Set(t3[:op1])
op1s_expected_3 = Set([:⋆,:Δ,:⋆⁻¹])
@test issetequal(op1s_3, op1s_expected_3)

# Test ASCII tangent operator identifies a TVar.
t4 = @decapode begin
  A == dt(B)
end
@test nparts(t4, :TVar) == 1

# Test vec_to_dec! on a single operator.
t5 = @decapode begin
  A == div(B)
end
vec_to_dec!(t5)

op1s_5 = Set(t5[:op1])
op1s_expected_5 = Set([[:⋆,:d,:⋆]])
@test issetequal(op1s_5, op1s_expected_5)

# Test divergence of gradient is the Laplacian.
t6 = @decapode begin
  A == ∘(grad, div)(B)
end
t6 = expand_operators(t6)
vec_to_dec!(t6)
t6 = contract_operators(t6)
@test only(t6[:op1]) == [:d,:⋆,:d,:⋆]

# Test curl of curl is a vector Laplacian.
t7 = @decapode begin
  A == ∘(∇x, ∇x)(B)
end
t7 = expand_operators(t7)
vec_to_dec!(t7)
t7 = contract_operators(t7)
@test only(t7[:op1]) == [:d,:⋆,:d,:⋆]

# Test advection is divergence of wedge product.
t8 = @decapode begin
  A == adv(B, C)
end
vec_to_dec!(t8)

@test only(t8[:op1]) == [:⋆,:d,:⋆]
@test only(t8[:op2]) == :∧

# Test multiple advections.
t9 = @decapode begin
  A == adv(B, C)
  D == adv(E, F)
end
vec_to_dec!(t9)

t9_expected = @decapode begin
  A == ∘(⋆,d,⋆)(B ∧ C)
  D == ∘(⋆,d,⋆)(E ∧ F)
end
t9_expected[incident(t9_expected, Symbol("•1"), :name), :name] = Symbol("•_adv_1")
t9_expected[incident(t9_expected, Symbol("•2"), :name), :name] = Symbol("•_adv_2")
@test is_isomorphic(t9, t9_expected)

end
