using Test
#
using DiagrammaticEquations
using DiagrammaticEquations.ThDEC
using DiagrammaticEquations.decapodes
#
using SymbolicUtils

@testset "ThDEC Signature checking" begin
    @test Scalar() + Scalar() == Scalar()
end

# load up some variable variables and expressions
a, b = @syms a::Real b::Real
u, v = @syms u::PrimalFormT{0} du::PrimalFormT{1}
ω, η = @syms ω::PrimalFormT{1} η::DualFormT{2}
ϕ, ψ = @syms ϕ::PrimalVFT ψ::DualVFT

expr_scalar_addition = a + b
expr_primal_wedge = ThDEC.:∧(ω, du)

@testset "Term Construction" begin
  
    # test conversion to underlying type
    @test Sort(a) == Scalar()
    @test Sort(u) == PrimalForm(0)
    @test Sort(ω) == PrimalForm(1)
    @test Sort(η) == DualForm(2)
    @test Sort(ϕ) == PrimalVF()
    @test Sort(ψ) == DualVF()

    @test_throws ThDEC.SortError ThDEC.♯(u)

    # test unary operator conversion to decaexpr
    @test Term(1) == DiagrammaticEquations.decapodes.Lit(Symbol("1"))
    @test Term(a) == Var(:a)
    @test Term(ThDEC.∂ₜ(u)) == Tan(Var(:u))
    @test Term(ThDEC.★(ω)) == App1(:★₁, Var(:ω))
    @test Term(ThDEC.♭(ψ)) == App1(:♭s, Var(:ψ)) 
    # @test Term(DiagrammaticEquations.ThDEC.♯(du))
    
    @test_throws ThDEC.SortError ThDEC.★(ϕ)
    
    # test binary operator conversion to decaexpr
    @test Term(a + b) == Plus(Term[Var(:a), Var(:b)])
    @test Term(a * b) == DiagrammaticEquations.decapodes.Mult(Term[Var(:a), Var(:b)])
    @test Term(ThDEC.:∧(ω, du)) == App2(:∧₁₁, Var(:ω), Var(:du)) 

end

@testset "Moving between DecaExpr and DecaSymbolic" begin end

context = Dict(:a => Scalar(), :b => Scalar()
               ,:u => PrimalForm(0), :du => PrimalForm(1))

js = [Judgement(:u, :Form0, :X)
      ,Judgement(:∂ₜu, :Form0, :X)
      ,Judgement(:Δu, :Form0, :X)]
eqs = [Eq(Var(:∂ₜu), AppCirc1([:⋆₂⁻¹, :d₁, :⋆₁, :d₀], Var(:u)))
       ,Eq(Tan(Var(:u)), Var(:∂ₜu))]
heat_eq = DecaExpr(js, eqs)


symb_heat_eq = DecaSymbolic(heat_eq)
deca_expr = DecaExpr(symb_heat_eq)

@test js == deca_expr.context

# eqs in the left has AppCirc1[vector, term]
# deca_expr.equations on the right has nested App1
# expected behavior is that nested AppCirc1 is preserved
@test_broken eqs == deca_expr.equations

# copied from test/language
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

