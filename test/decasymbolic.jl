using Test

using DiagrammaticEquations
using DiagrammaticEquations.ThDEC
using DiagrammaticEquations.decapodes

using SymbolicUtils

# what space are we working in?
X = Space(:X, 2)

lookup = SpaceLookup(X)

# load up some variable variables and expressions
a, b = @syms a::Real b::Real
u, v = @syms u::PrimalFormT{0, :X, 2} du::PrimalFormT{1, :X, 2}
ω, η = @syms ω::PrimalFormT{1, :X, 2} η::DualFormT{2, :X, 2}
ϕ, ψ = @syms ϕ::PrimalVFT{:X, 2} ψ::DualVFT{:X, 2}
# TODO would be nice to pass the space globally to avoid duplication

@testset "Term Construction" begin
  
    # test conversion to underlying type
    @test Sort(a) == Scalar()
    @test Sort(u) == PrimalForm(0, X)
    @test Sort(ω) == PrimalForm(1, X)
    @test Sort(η) == DualForm(2, X)
    @test Sort(ϕ) == PrimalVF(X)
    @test Sort(ψ) == DualVF(X)

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

context = Dict(:a => Scalar()
                    ,:b => Scalar()
                    ,:u => PrimalForm(0, X)
                    ,:du => PrimalForm(1, X))

js = [Judgement(:u, :Form0, :X)
        ,Judgement(:∂ₜu, :Form0, :X)
        ,Judgement(:Δu, :Form0, :X)]
eqs = [Eq(Var(:∂ₜu)
        , AppCirc1([:⋆₂⁻¹, :d₁, :⋆₁, :d₀], Var(:u)))
        , Eq(Tan(Var(:u)), Var(:∂ₜu))]
heat_eq = DecaExpr(js, eqs)

symb_heat_eq = DecaSymbolic(lookup, heat_eq)
deca_expr = DecaExpr(symb_heat_eq)

@testset "Moving between DecaExpr and DecaSymbolic" begin
    
    @test js == deca_expr.context

    # eqs in the left has AppCirc1[vector, term]
    # deca_expr.equations on the right has nested App1
    # expected behavior is that nested AppCirc1 is preserved
    @test_broken eqs == deca_expr.equations
    # use expand_operators to get rid of parentheses
    # infer_types and resolve_overloads
    
end

# convert both into ACSets then is_iso them
@testset "" begin

    Σ = DiagrammaticEquations.SummationDecapode(deca_expr)
    Δ = DiagrammaticEquations.SummationDecapode(symb_heat_eq)
    @test Σ == Δ


end
