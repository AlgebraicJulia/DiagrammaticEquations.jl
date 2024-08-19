module ThDEC

using MLStyle
using StructEquality

import Base: +, -, *

struct SortError <: Exception
    message::String
end

@struct_hash_equal struct Space
    name::Symbol
    dim::Int
end
export Space

dim(s::Space) = s.dim
Base.nameof(s::Space) = s.name

struct SpaceLookup
    default::Space
    named::Dict{Symbol,Space}
end
export SpaceLookup

@data Sort begin
    Scalar()
    Form(dim::Int, isdual::Bool, space::Space)
    VField(isdual::Bool, space::Space)
end
export Sort, Scalar, Form, VField

function fromexpr(lookup::SpaceLookup, e, ::Type{Sort})
    (name, spacename) = @match e begin
        name::Symbol => (name, nothing)
        :($name{$spacename}) => (name, spacename)
    end
    space = @match spacename begin
        ::Nothing => lookup.default
        name::Symbol => lookup.named[name]
    end
    @match name begin
        :Form0 => Form(0, false, space)
        :Form1 => Form(1, false, space)
        :Form2 => Form(2, false, space)
        :DualForm0 => Form(0, true, space)
        :DualForm1 => Form(1, true, space)
        :DualForm2 => Form(2, true, space)
        :Constant => Scalar()
    end
end

Base.nameof(s::Scalar) = :Constant

function Base.nameof(f::Form; with_dim_parameter=false)
    dual = isdual(f) ? "Dual" : ""
    formname = Symbol("$(dual)Form$(dim(f))")
    if with_dim_parameter
        return Expr(:curly, formname, dim(space(f)))
    else
        return formname
    end
end

const VF = VField

dim(ω::Form) = ω.dim
isdual(ω::Form) = ω.isdual
space(ω::Form) = ω.space
export space

isdual(v::VField) = v.isdual
space(v::VField) = v.space

# convenience functions
PrimalForm(i::Int, space::Space) = Form(i, false, space)
export PrimalForm

DualForm(i::Int, space::Space) = Form(i, true, space)
export DualForm

PrimalVF(space::Space) = VF(false, space)
export PrimalVF

DualVF(space::Space) = VF(true, space)
export DualVF

# show methods
show_duality(ω::Form) = isdual(ω) ? "dual" : "primal"

function Base.show(io::IO, ω::Form)
    print(io, isdual(ω) ? "DualForm($(dim(ω))) on $(space(ω))" : "PrimalForm($(dim(ω))) on $(space(ω))")
end

# TODO: VField
@nospecialize
function +(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (Scalar(), Scalar()) => Scalar()
        (Scalar(), Form(i, isdual, space)) ||
            (Form(i, isdual, space), Scalar()) => Form(i, isdual, space)
        (Form(i1, isdual1, space1), Form(i2, isdual2, space2)) =>
            if (i1 == i2) && (isdual1 == isdual2) && (space1 == space2)
                Form(i1, isdual1)
            else
                throw(SortError(
                    """
                    Can not add two forms of different dimensions/dualities/spaces:
                    $((i1,isdual1,space1)) and $((i2,isdual2,space2))
                    """)
                )
            end
    end
end

# Type-checking inverse of addition follows addition
-(s1::Sort, s2::Sort) = +(s1, s2)

# TODO error for Forms

# Negation is always valid
-(s::Sort) = s

# TODO: VField
@nospecialize
function *(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (Scalar(), Scalar()) => Scalar()
        (Scalar(), Form(i, isdual, space)) ||
            (Form(i, isdual, space), Scalar()) => Form(i, isdual)
        (Form(_, _, _), Form(_, _, _)) =>
            throw(SortError("Cannot scalar multiply a form with a form. Maybe try `∧`??"))
    end
end

const SUBSCRIPT_DIGIT_0 = '₀'

function as_sub(n::Int)
    join(map(d -> SUBSCRIPT_DIGIT_0 + d, digits(n)))
end

# TODO: VField
@nospecialize
function ∧(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (Form(i, isdual, space), Scalar()) || (Scalar(), Form(i, isdual, space)) =>
            Form(i, isdual, space)
        (Form(i1, isdual1, space1), Form(i2, isdual2, space2)) => begin
            (isdual1 == isdual2) && (space1 == space2) || throw(SortError("Can only take a wedge product of two forms of the same duality on the same space"))
            if i1 + i2 <= dim(space1)
                Form(i1 + i2, isdual1, space1)
            else
                throw(SortError("Can only take a wedge product when the dimensions of the forms add to less than n, where n = $(dim(space1)) is the dimension of the ambient space: tried to wedge product $i1 and $i2"))
            end
        end
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
        Form(i, isdual, space) =>
            if i < dim(space)
                Form(i + 1, isdual, space)
            else
                throw(SortError("Cannot take exterior derivative of a k-form for k >= n, where n = $(dim(space)) is the dimension of its ambient space"))
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
        VF(isdual, space) => throw(SortError("Cannot take the Hodge star of a vector field"))
        Form(i, isdual, space) => Form(dim(space) - i, !isdual, space)
    end
end

function Base.nameof(::typeof(★), s)
    inv = isdual(s) ? "⁻¹" : ""
    Symbol("★$(as_sub(isdual(s) ? dim(space(s)) - dim(s) : dim(s)))$(inv)")
end

@nospecialize
function ι(s1::Sort, s2::Sort)
    @match (s1, s2) begin
        (VF(true, space), Form(i, true, space)) => Form(i, false, space) # wrong
        (VF(true, space), Form(i, false, space)) => DualForm(i, true, space)
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
        Form(1, isdual, space) => VF(isdual, space)
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
        VF(true, space) => Form(1, false, space)
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
        Form(i, isdual, space) => Form(i, !isdual, space)
        _ => throw(SortError("♭♯ is only defined on forms."))
    end
end

# Δ = ★d⋆d, but we check signature here to throw a more helpful error
function Δ(s::Sort)
    @match s begin
        Form(0, isdual, space) => Form(0, isdual, space)
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
