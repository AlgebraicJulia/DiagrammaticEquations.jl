module ThDEC

using MLStyle

import Base: +, -, *

struct SortError <: Exception
    message::String
end

@data Sort begin
    Scalar()
    Form(dim::Int, isdual::Bool)
    VField(isdual::Bool)
end
export Sort, Scalar, Form, VField

const SORT_LOOKUP = Dict(
    :Form0 => Form(0, false),
    :Form1 => Form(1, false),
    :Form2 => Form(2, false),
    :DualForm0 => Form(0, true),
    :DualForm1 => Form(1, true),
    :DualForm2 => Form(2, true),
    :Constant => Scalar()
)

function Base.nameof(s::Scalar)
    :Constant
end

function Base.nameof(f::Form)
    dual = isdual(f) ? "Dual" : ""
    Symbol("$(dual)Form$(dim(f))")
end

const VF = VField

dim(ω::Form) = ω.dim
isdual(ω::Form) = ω.isdual

isdual(v::VField) = v.isdual

# convenience functions
PrimalForm(i::Int) = Form(i, false)
export PrimalForm

DualForm(i::Int) = Form(i, true)
export DualForm

PrimalVF() = VF(false)
export PrimalVF

DualVF() = VF(true)
export DualVF

# show methods
show_duality(ω::Form) = isdual(ω) ? "dual" : "primal"

function Base.show(io::IO, ω::Form)
    print(io, isdual(ω) ? "DualForm($(dim(ω)))" : "PrimalForm($(dim(ω)))")
end

@nospecialize
function +(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (Scalar(), Scalar()) => Scalar()
        (Scalar(), Form(i, isdual)) ||
            (Form(i, isdual), Scalar()) => Form(i, isdual)
        (Form(i1, isdual1), Form(i2, isdual2)) =>
            if (i1 == i2) && (isdual1 == isdual2)
                Form(i1, isdual1)
            else
                throw(SortError("Cannot add two forms of different dimensions/dualities: $((i1,isdual1)) and $((i2,isdual2))"))
            end
    end
end

# Type-checking inverse of addition follows addition
-(s1::Sort, s2::Sort) = +(s1, s2)

# TODO error for Forms

# Negation is always valid
-(s::Sort) = s

@nospecialize
function *(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (Scalar(), Scalar()) => Scalar()
        (Scalar(), Form(i, isdual)) ||
            (Form(i, isdual), Scalar()) => Form(i, isdual)
        (Form(_, _), Form(_, _)) => throw(SortError("Cannot scalar multiply a form with a form. Maybe try `∧`??"))
    end
end

const SUBSCRIPT_DIGIT_0 = '₀'

function as_sub(n::Int)
    join(map(d -> SUBSCRIPT_DIGIT_0 + d, digits(n)))
end

@nospecialize
function ∧(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (Form(i, isdual), Scalar()) || (Scalar(), Form(i, isdual)) => Form(i, isdual)
        (Form(i1, isdual), Form(i2, isdual)) =>
            if i1 + i2 <= 2
                Form(i1 + i2, isdual)
            else
                throw(SortError("Can only take a wedge product when the dimensions of the forms add to less than 2: tried to wedge product $i1 and $i2"))
            end
        _ => throw(SortError("Can only take a wedge product of two forms of the same duality"))
    end
end

function Base.nameof(::typeof(∧), s1, s2)
    Symbol("∧$(as_sub(dim(s1)))$(as_sub(dim(s2)))")
end

@nospecialize
∂ₜ(s::Sort) = s

@nospecialize
function d(s::Sort)
    @match s begin
        Scalar() => throw(SortError("Cannot take exterior derivative of a scalar"))
        Form(i, isdual) =>
            if i <= 1
                Form(i + 1, isdual)
            else
                throw(SortError("Cannot take exterior derivative of a n-form for n >= 1"))
            end
    end
end

function Base.nameof(::typeof(d), s)
    Symbol("d$(as_sub(dim(s)))")
end

@nospecialize
function ★(s::Sort)
    @match s begin
        Scalar() => throw(SortError("Cannot take Hodge star of a scalar"))
        Form(i, isdual) => Form(2 - i, !isdual)
        VF(isdual) => throw(SortError("Cannot take the Hodge star of a vector field"))
    end
end

function Base.nameof(::typeof(★), s)
    inv = isdual(s) ? "⁻¹" : ""
    Symbol("★$(as_sub(isdual(s) ? 2 - dim(s) : dim(s)))$(inv)")
end

@nospecialize
function ι(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (VF(true), Form(i, true)) => PrimalForm() # wrong
        (VF(true), Form(i, false)) => DualForm()
        _ => throw(SortError("Can only define the discrete interior product on:
                PrimalVF, DualForm(i)
                DualVF(), PrimalForm(i)
                ."))
    end
end

# in practice, a scalar may be treated as a constant 0-form.
function ♯(s::Sort)
    @match s begin
        Scalar() => PrimalVF()
        Form(1, isdual) => VF(isdual)
        _ => throw(SortError("Can only take ♯ to 1-forms"))
    end
end
# musical isos may be defined for any combination of (primal/dual) form -> (primal/dual) vf.

# TODO
function Base.nameof(::typeof(♯), s)
    Symbol("♯s")
end


function ♭(s::Sort)
    @match s begin
        VF(true) => PrimalForm(1)
        _ => throw(SortError("Can only apply ♭ to dual vector fields"))
    end
end

# TODO
function Base.nameof(::typeof(♭), s)
    Symbol("♭s")
end

# OTHER

function ♭♯(s::Sort)
    @match s begin
        Form(i, isdual) => Form(i, !isdual)
        _ => throw(SortError("♭♯ is only defined on forms."))
    end
end

# Δ = ★d⋆d, but we check signature here to throw a more helpful error
function Δ(s::Sort)
    @match s begin
        Form(0, isdual) => Form(0, isdual)
        _ => throw(SortError("Δ is not defined for $s"))
    end
end

const OPERATOR_LOOKUP = Dict(
    :⋆₀ => ★,
    :⋆₁ => ★,
    :⋆₂ => ★,

    # Inverse Hodge Stars
    :⋆₀⁻¹ => ★,
    :⋆₁⁻¹ => ★,
    :⋆₂⁻¹ => ★,

    # Differentials
    :d₀ => d,
    :d₁ => d,

    # Dual Differentials
    :dual_d₀ => d,
    :d̃₀ => d,
    :dual_d₁ => d,
    :d̃₁ => d,

    # Wedge Products
    :∧₀₁ => ∧,
    :∧₁₀ => ∧,
    :∧₀₂ => ∧,
    :∧₂₀ => ∧,
    :∧₁₁ => ∧,

    # Primal-Dual Wedge Products
    :∧ᵖᵈ₁₁ => ∧,
    :∧ᵖᵈ₀₁ => ∧,
    :∧ᵈᵖ₁₁ => ∧,
    :∧ᵈᵖ₁₀ => ∧,

    # Dual-Dual Wedge Products
    :∧ᵈᵈ₁₁ => ∧,
    :∧ᵈᵈ₁₀ => ∧,
    :∧ᵈᵈ₀₁ => ∧,

    # Dual-Dual Interior Products
    :ι₁₁ => ι,
    :ι₁₂ => ι,

    # Dual-Dual Lie Derivatives
    # :ℒ₁ => ℒ,

    # Dual Laplacians
    # :Δᵈ₀ => Δ,
    # :Δᵈ₁ => Δ,

    # Musical Isomorphisms
    :♯ => ♯,
    :♯ᵈ => ♯, :♭ => ♭,

    # Averaging Operator
    # :avg₀₁ => avg,

    # Negatives
    :neg => -,

    # Basics

    :- => -,
    :+ => +,
    :* => *,
    :/ => /,
    :.- => .-,
    :.+ => .+,
    :.* => .*,
    :./ => ./,
)

end
