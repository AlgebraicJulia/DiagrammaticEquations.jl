using Test
using Catlab
using Catlab.Theories
using Catlab.CategoricalAlgebra
using Catlab.WiringDiagrams
using Catlab.WiringDiagrams.DirectedWiringDiagrams
using Catlab.Graphics
using Catlab.Programs
using CombinatorialSpaces
using CombinatorialSpaces.ExteriorCalculus
using LinearAlgebra
using MLStyle
using Base.Iterators

using DiagrammaticEquations

@testset "Parsing" begin

  # @present DiffusionSpace2D(FreeExtCalc2D) begin
  #   X::Space
  #   k::Hom(Form1(X), Form1(X)) # diffusivity of space, usually constant (scalar multiplication)
  #   proj₁_⁰⁰₀::Hom(Form0(X) ⊗ Form0(X), Form0(X))
  #   proj₂_⁰⁰₀::Hom(Form0(X) ⊗ Form0(X), Form0(X))
  #   sum₀::Hom(Form0(X) ⊗ Form0(X), Form0(X))
  #   prod₀::Hom(Form0(X) ⊗ Form0(X), Form0(X))
  # end
  
  
  # Diffusion = @decapode DiffusionSpace2D begin
  #     (C, Ċ₁, Ċ₂)::Form0{X}
  #     Ċ₁ == ⋆₀⁻¹{X}(dual_d₁{X}(⋆₁{X}(k(d₀{X}(C)))))
  #     Ċ₂ == ⋆₀⁻¹{X}(dual_d₁{X}(⋆₁{X}(d₀{X}(C))))
  #     ∂ₜ{Form0{X}}(C) == Ċ₁ + Ċ₂
  # end
  
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

