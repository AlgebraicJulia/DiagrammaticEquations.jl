using Test
using DiagrammaticEquations
using DiagrammaticEquations.Deca.ThDEC
using DiagrammaticEquations.decapodes
using SymbolicUtils
using SymbolicUtils: symtype, promote_symtype, Symbolic
using MLStyle

# load up some variable variables and expressions
ℓ,   = @syms ℓ::Literal
c, t = @syms c::ConstScalar t::Parameter
a, b = @syms a::Scalar b::Scalar
u, v = @syms u::PrimalForm{0, :X, 2} du::PrimalForm{1, :X, 2}
ω, η = @syms ω::PrimalForm{1, :X, 2} η::DualForm{2, :X, 2}
ϕ, ψ = @syms ϕ::PrimalVF{:X, 2} ψ::DualVF{:X, 2}
# TODO would be nice to pass the space globally to avoid duplication

@testset "Term Construction" begin

    @test symtype(ℓ) == Literal
    @test symtype(c) == ConstScalar
    @test symtype(t) == Parameter
    @test symtype(a) == Scalar
    
    @test symtype(u) == PrimalForm{0, :X, 2}
    @test symtype(ω) == PrimalForm{1, :X, 2}
    @test symtype(η) == DualForm{2, :X, 2}
    @test symtype(ϕ) == PrimalVF{:X, 2}
    @test symtype(ψ) == DualVF{:X, 2}

    @test symtype(c + t) == Scalar
    @test symtype(t + t) == Scalar
    @test symtype(c + c) == Scalar

    @test symtype(u ∧ ω) == PrimalForm{1, :X, 2}
    @test symtype(ω ∧ ω) == PrimalForm{2, :X, 2}
    # @test_throws ThDEC.SortError ThDEC.♯(u)
    @test symtype(Δ(u) + Δ(u)) == PrimalForm{0, :X, 2}

    # test unary operator conversion to decaexpr
    @test Term(1) == Lit(Symbol("1"))
    @test Term(a) == Var(:a)
    @test Term(c) == Var(:c)
    @test Term(t) == Var(:t)
    @test Term(∂ₜ(u)) == Tan(Var(:u))
    @test Term(★(ω)) == App1(:★₁, Var(:ω))
    
    # test binary operator conversion to decaexpr
    @test Term(a + b) == Plus(Term[Var(:a), Var(:b)])
    @test Term(a * b) == Mult(Term[Var(:a), Var(:b)])
    @test Term(ω ∧ du) == App2(:∧₁₁, Var(:ω), Var(:du))
 
    # test promoting types
    @test promote_symtype(d, u) == PrimalForm{1, :X, 2}
    @test promote_symtype(+, a, b) == Scalar
    @test promote_symtype(∧, u, u) == PrimalForm{0, :X, 2}
    @test promote_symtype(∧, u, ω) == PrimalForm{1, :X, 2}
    @test promote_symtype(-, a) == Scalar
    @test promote_symtype(-, u, u) == PrimalForm{0, :X, 2}

    # test composition
    @test promote_symtype(d ∘ d, u) == PrimalForm{2, :X, 2}

end

@testset "Operator definition" begin

    # this is not nabla but "bizarro Δ"
    del_expand_0, del_expand_1 = 
    @operator ∇(S)::DECQuantity begin
        @match S begin
            PatScalar(_) => error("Argument of type $S is invalid")
            PatForm(_) => promote_symtype(★ ∘ d ∘ ★ ∘ d, S)
        end
        @rule ∇(~x::isForm0) => ★(d(★(d(~x))))
        @rule ∇(~x::isForm1) => ★(d(★(d(~x)))) + d(★(d(★(~x))))
    end;

    @test_throws Exception ∇(b)
    @test symtype(∇(u)) == PrimalForm{0, :X ,2}
    @test promote_symtype(∇, u) == PrimalForm{0, :X, 2}

    @test isequal(del_expand_0(∇(u)), ★(d(★(d(u)))))

    # we will test is new operator
    (r0, r1, r2) = @operator ρ(S)::DECQuantity begin
        if S <: Form
            Scalar
        else
            Form
        end
        @rule ρ(~x::isForm0) => 0
        @rule ρ(~x::isForm1) => 1
        @rule ρ(~x::isForm2) => 2
    end

    @test symtype(ρ(u)) == Scalar

    R, = @operator φ(S1, S2, S3)::DECQuantity begin
        let T1=S1, T2=S2, T3=S3
            Scalar
        end
        @rule φ(2(~x::isForm0), 2(~y::isForm0), 2(~z::isForm0)) => 2*φ(~x,~y,~z)
    end

    # TODO we need to alias rewriting rules
    @alias (φ′,) => φ

    @test isequal(R(φ(2u,2u,2u)), R(φ′(2u,2u,2u)))

end

@testset "Conversion" begin

    context = Dict(:a => Scalar(),:b => Scalar()
                    ,:u => PrimalForm(0, X),:du => PrimalForm(1, X))
    js = [Judgement(:u, :Form0, :X)
        ,Judgement(:∂ₜu, :Form0, :X)
        ,Judgement(:Δu, :Form0, :X)]
    eqs = [Eq(Var(:∂ₜu)
        , AppCirc1([:⋆₂⁻¹, :d₁, :⋆₁, :d₀], Var(:u)))
        , Eq(Tan(Var(:u)), Var(:∂ₜu))]
    heat_eq = DecaExpr(js, eqs)

    symb_heat_eq = DecaSymbolic(lookup, heat_eq)
    deca_expr = DecaExpr(symb_heat_eq)

end

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
