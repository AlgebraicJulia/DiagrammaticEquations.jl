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
h,    = @syms h::PrimalForm{2, :X, 2}
ϕ, ψ  = @syms ϕ::PrimalVF{:X, 2} ψ::DualVF{:X, 2}
# TODO would be nice to pass the space globally to avoid duplication

u2,   = @syms u2::PrimalForm{0, :Y, 2}
u3,   = @syms u3::PrimalForm{0, :X, 3}

@testset "Symtypes" begin

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

    @test symtype(-(ϐ)) == InferredType
    @test symtype(d(ϐ)) == InferredType
    @test symtype(★(ϐ)) == InferredType
    @test symtype(Δ(ϐ)) == InferredType

    @test symtype(ϐ + ϐ) == InferredType
    @test symtype(ϐ + u) == InferredType
    @test_throws OperatorError symtype(u + du + ϐ)
    # The order of the addition counts when type checking, probably applies
    # to other operations as well
    @test_broken (try symtype(ϐ + u + du) catch e; e; end) isa Exception

    @test symtype(ϐ - u) == InferredType
    @test symtype(ω * ϐ) == InferredType
    @test symtype(ϐ ∧ ω) == InferredType
end

@testset "Type Information" begin

  @test dim(symtype(η)) == 2

  @test isdual(symtype(η))
  @test !isdual(symtype(u))

  @test space(symtype(η)) == :X
  @test spacedim(symtype(η)) == 2

  @test isForm0(u)
  @test !isForm0(du)
  @test !isForm0(a)

  @test isForm1(ω)
  @test !isForm1(u)
  @test !isForm1(a)

  @test isForm2(η)
  @test !isForm2(u)
  @test !isForm2(a)

  @test isDualForm(η)
  @test !isDualForm(u)
  @test !isDualForm(a)
end

@testset "Nameof" begin
  @test nameof(symtype(c)) == :Constant
  @test nameof(symtype(t)) == :Parameter
  @test nameof(symtype(a)) == :Scalar
  @test nameof(symtype(ℓ)) == :Literal

  @test nameof(symtype(u)) == :Form0
  @test nameof(symtype(ω)) == :Form1
  @test nameof(symtype(η)) == :DualForm2

  @test nameof(-, symtype(u)) == Symbol("-")
  @test nameof(-, symtype(u), symtype(u)) == Symbol("-")
  @test nameof(-, symtype(a), symtype(b)) == Symbol("-")

  @test nameof(∧, symtype(u), symtype(u)) == Symbol("∧₀₀")
  @test nameof(∧, symtype(u), symtype(ω)) == Symbol("∧₀₁")
  @test nameof(∧, symtype(ω), symtype(u)) == Symbol("∧₁₀")

  # TODO: Do we need a special designation for wedges with duals in them?
  @test nameof(∧, symtype(ω), symtype(η)) == Symbol("∧₁₂")

  @test nameof(∂ₜ, symtype(u)) == Symbol("∂ₜ")
  @test nameof(∂ₜ, symtype(d(u))) == Symbol("∂ₜ")

  @test nameof(d, symtype(u)) == Symbol("d₀")
  @test nameof(d, symtype(η)) == Symbol("dual_d₂")

  @test nameof(Δ, symtype(u)) == Symbol("Δ₀")
  @test nameof(Δ, symtype(ω)) == Symbol("Δ₁")

  @test nameof(★, symtype(u)) == Symbol("★₀")
  @test nameof(★, symtype(ω)) == Symbol("★₁")
  @test nameof(★, symtype(η)) == Symbol("★₀⁻¹")
end

@testset "Symtype Promotion" begin
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

@testset "Term Construction" begin

  # test unary operator conversion to decaexpr
  @test Term(1) == Lit(Symbol("1"))
  @test Term(a) == Var(:a)
  @test Term(c) == Var(:c)
  @test Term(t) == Var(:t)
  @test Term(∂ₜ(u)) == Tan(Var(:u))
  @test_broken Term(∂ₜ(u)) == Term(DerivOp(u))

  @test Term(★(ω)) == App1(:★₁, Var(:ω))
  @test Term(★(η)) == App1(:★₀⁻¹, Var(:η))

  # The symbolics no longer captures ∘
  @test Term(∘(d, d)(u)) == App1(:d₁, App1(:d₀, Var(:u)))

  # test binary operator conversion to decaexpr
  @test Term(a + b) == Plus(Term[Var(:a), Var(:b)])

  # TODO: Currently parses as addition
  @test_broken Term(a - b) == App2(:-, Var(:a), Var(:b))
  @test Term(a * b) == Mult(Term[Var(:a), Var(:b)])
  @test Term(ω ∧ du) == App2(:∧₁₁, Var(:ω), Var(:du))

  @test Term(ω + du + d(u)) == Plus(Term[App1(:d₀, Var(:u)), Var(:du), Var(:ω)])

  let
    @syms f(x, y, z)
    @test_throws "was unable to convert" Term(f(a, b, u))
  end

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

@testset "Errors" begin

    # addition
    @test_throws OperatorError u + du # mismatched grade
    @test_throws OperatorError h + η  # primal and dual
    @test_throws OperatorError u + u2 # differing spaces
    @test_throws OperatorError u + u3 # differing codimension

    # subtraction
    @test_throws OperatorError u - du # mismatched grade
    @test_throws OperatorError h - η  # primal and dual
    @test_throws OperatorError u - u2 # differing spaces
    @test_throws OperatorError u - u3 # differing spatial dimension

    # exterior derivative
    @test_throws OperatorError d(a)
    @test_throws OperatorError d(ϕ)

    # hodge star
    @test_throws OperatorError ★(a)
    @test_throws OperatorError ★(ϕ)

    # Laplacian
    @test_throws OperatorError Δ(a)
    @test_throws OperatorError Δ(ϕ)

    # multiplication
    @test_throws OperatorError u * du
    @test_throws OperatorError ϕ * u

    @test_throws OperatorError du ∧ h # checks if spaces exceed dimension
    @test_throws OperatorError a ∧ a  # cannot take wedge of scalars
    @test_throws OperatorError u ∧ ϕ  # cannot take wedge of vector fields

end

@testset "Conversion" begin

    roundtrip(d::SummationDecapode) = SummationDecapode(DecaExpr(SymbolicContext(Term(d))))

    just_vars = @decapode begin
      u::Form0
      v::Form0
    end
    @test just_vars == roundtrip(just_vars)

    with_tan = @decapode begin
        u::Form0
        v::Form0
        ∂ₜ(v) == u
    end
    @test with_tan == roundtrip(with_tan)

    with_op2 = @decapode begin
      u::Form0
      v::Form1
      w::Form1

      w == ∧₀₁(u, v)
    end
    @test with_op2 == roundtrip(with_op2)

    with_mult = @decapode begin
      u::Form1
      v::Form1

      v == u * 2
    end
    @test with_mult == roundtrip(with_mult)

    with_circ = @decapode begin
      u::Form0
      v::Form2
      v == ∘(d₁, d₀)(u)
    end
    with_circ_expanded = @decapode begin
      u::Form0
      v::Form2
      v == d₁(d₀(u))
    end
    @test with_circ_expanded == roundtrip(with_circ)

    with_infers = @decapode begin
      v::Form1

      w == ∧(v, u)
    end
    # Base.nameof doesn't yet support taking InferredTypes
    @test with_infers == roundtrip(with_infers)

    Heat = @decapode begin
        u::Form0
        v::Form0
        κ::Constant
        ∂ₜ(v) == Δ₀(u)*κ
    end
    infer_types!(Heat)
    @test Heat == roundtrip(Heat)

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
