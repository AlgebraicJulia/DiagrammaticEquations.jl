using Test
using DiagrammaticEquations
using DiagrammaticEquations.Deca.ThDEC
using DiagrammaticEquations.decapodes
using SymbolicUtils
using SymbolicUtils: symtype, promote_symtype, Symbolic
using MLStyle

import DiagrammaticEquations: rules

# load up some variable variables and expressions
ϐ,    = @syms ϐ::InferredType # \varbeta
ℓ,    = @syms ℓ::Literal
c, t  = @syms c::Const t::Parameter
a, b  = @syms a::Scalar b::Scalar
u, du = @syms u::PrimalForm{0, :X, 2} du::PrimalForm{1, :X, 2}
ω, η  = @syms ω::PrimalForm{1, :X, 2} η::DualForm{2, :X, 2}
ϕ, ψ  = @syms ϕ::PrimalVF{:X, 2} ψ::DualVF{:X, 2}
# TODO would be nice to pass the space globally to avoid duplication

@testset "Term Construction" begin

    @test symtype(ϐ) == InferredType
    @test symtype(ℓ) == Literal
    @test symtype(c) == Const
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
    @test symtype(t + ϐ) == InferredType

    @test symtype(u ∧ ω) == PrimalForm{1, :X, 2}
    @test symtype(ω ∧ ω) == PrimalForm{2, :X, 2}
    @test symtype(u ∧ ϐ) == InferredType

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

# this is not nabla but "bizarro Δ"
del_expand_0, del_expand_1 = @operator ∇(S)::DECQuantity begin
    @match S begin
        PatScalar(_) => error("Argument of type $S is invalid")
        PatForm(_) => promote_symtype(★ ∘ d ∘ ★ ∘ d, S)
    end
    @rule ∇(~x::isForm0) => ★(d(★(d(~x))))
    @rule ∇(~x::isForm1) => ★(d(★(d(~x)))) + d(★(d(★(~x))))
end;

# we will test is new operator
(r0, r1, r2) = @operator ρ(S)::DECQuantity begin
    S <: Form ? Scalar : Form
    @rule ρ(~x::isForm0) => 0
    @rule ρ(~x::isForm1) => 1
    @rule ρ(~x::isForm2) => 2
end

R, = @operator φ(S1, S2, S3)::DECQuantity begin
    let T1=S1, T2=S2, T3=S3
        Scalar
    end
    @rule φ(2(~x::isForm0), 2(~y::isForm0), 2(~z::isForm0)) => 2*φ(~x,~y,~z)
end

@alias (φ′,) => φ

@testset "Operator definition" begin

    # ∇
    @test_throws Exception ∇(b)
    @test symtype(∇(u)) == PrimalForm{0, :X ,2}
    @test promote_symtype(∇, u) == PrimalForm{0, :X, 2}
    @test isequal(del_expand_0(∇(u)), ★(d(★(d(u)))))
    
    # ρ
    @test symtype(ρ(u)) == Scalar

    # R
    @test isequal(R(φ(2u,2u,2u)), R(φ′(2u,2u,2u)))
    # TODO we need to alias rewriting rules

end

@testset "Conversion" begin

    Exp = @decapode begin
        u::Form0
        v::Form0
        ∂ₜ(v) == u
    end
    context = SymbolicContext(Term(Exp))
    Exp′ = SummationDecapode(DecaExpr(context))

    # does roundtripping work
    @test Exp == Exp′

    Heat = @decapode begin
        u::Form0
        v::Form0
        κ::Constant
        ∂ₜ(v) == Δ(u)*κ
    end
    infer_types!(Heat)
    context = SymbolicContext(Term(Heat))
    Heat′ = SummationDecapode(DecaExpr(context))

    @test Heat == Heat′

    TumorInvasion = @decapode begin
        (C,fC)::Form0
        (Dif,Kd,Cmax)::Constant
        ∂ₜ(C) == Dif * Δ(C) + fC - C * Kd
    end
    infer_types!(TumorInvasion)
    context = SymbolicContext(Term(TumorInvasion))
    TumorInvasion′ = SummationDecapode(DecaExpr(context))

    # new terms introduced because Symbolics converts subtraction expressions
    # e.g., a - b => +(a, -b)
    @test_broken TumorInvasion == TumorInvasion′
    # TI' has (11, Literal, -1) and (12, infer, mult_1)
    # Op1 (2, 1, 4, 7) should be (2, 4, 1, 7)
    # Sum is (1, 6), (2, 10)

end
