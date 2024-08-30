module ThDEC

using ..DiagrammaticEquations: @operator, @alias, Quantity

using MLStyle
using StructEquality
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic, FnType, Sym, Term, symtype

import Base: +, -, *
import Catlab: Δ, ∧

# ##########################
# DECQuantity
#
# Type necessary for symbolic utils
# ##########################

abstract type DECQuantity <: Quantity end

struct Scalar <: DECQuantity end
export Scalar

struct FormParams
    dim::Int
    duality::Bool
    space::Symbol
    spacedim::Int
end

dim(fp::FormParams) = fp.dim
duality(fp::FormParams) = fp.duality
space(fp::FormParams) = fp.space
spacedim(fp::FormParams) = fp.spacedim

"""
i: dimension: 0,1,2, etc.
d: duality: true = dual, false = primal
s: name of the space (a symbol)
n: dimension of the space
"""
struct Form{i,d,s,n} <: DECQuantity end
export Form

# parameter accessors
dim(::Type{<:Form{i,d,s,n}}) where {i,d,s,n} = i
isdual(::Type{<:Form{i,d,s,n}}) where {i,d,s,n} = d
space(::Type{<:Form{i,d,s,n}}) where {i,d,s,n} = s
spacedim(::Type{<:Form{i,d,s,n}}) where {i,d,s,n} = n

export dim, isdual, space, spacedim

# convert form to form params
FormParams(::Type{<:Form{i,d,s,n}}) where {i,s,d,n} = FormParams(i,d,s,n)

struct VField{d,s,n} <: DECQuantity end
export VField

# parameter accessors
isdual(::Type{<:VField{d,s,n}}) where {d,s,n} = d
space(::Type{VField{d,s,n}}) where {d,s,n} = s
spacedim(::Type{VField{d,s,n}}) where {d,s,n} = n

# convenience functions
const PrimalForm{i,s,n} = Form{i,false,s,n}
export PrimalForm

const DualForm{i,s,n} = Form{i,true,s,n}
export DualForm

const PrimalVF{s,n} = VField{false,s,n}
export PrimalVF

const DualVF{s,n} = VField{true,s,n}
export DualVF

# ACTIVE PATTERNS

@active ActForm(T) begin
    if T <: Form
        Some(T)
    end
end

@active ActFormParams(T) begin
    if T <: Form
        Some([T.parameters...])
    end
end

@active ActFormDim(T) begin
    if T <: Form
        Some(dim(T))
    end
end

@active ActScalar(T) begin
    if T <: Scalar
        Some(T)
    end
end

@active ActVFParams(T) begin
    if T <: VField
        Some([T.parameters...])
    end
end

# HERE WE DEFINE THE SYMBOLICUTILS

# for every unary operator in our theory, take a BasicSymbolic type, convert its type parameter to a Sort in our theory, and return a term
unops = [:♯, :♭]

@operator -(S)::DECQuantity begin S end

@operator ∂ₜ(S)::DECQuantity begin S end

@operator d(S)::DECQuantity begin
    @match S begin
        ActFormParams([i,d,s,n]) => Form{i+1,d,s,n}
        _ => throw(SortError("Cannot apply the exterior derivative to $S"))
    end
end

@alias (d₀, d₁) => d

@operator ⋆(S)::DECQuantity begin
    @match S begin
        ActFormParams([i,d,s,n]) => Form{n-i,d,s,n}
        _ => throw(SortError("Cannot take the hodge star of $S"))
    end
end

@alias (⋆₀, ⋆₁, ⋆₂, ⋆₀⁻¹, ⋆₁⁻¹, ⋆₂⁻¹) => ⋆

@operator Δ(S)::DECQuantity begin
    @match S begin
        ActForm(x) => ⋆(d(⋆(d(x))))
        _ => throw(SortError("Cannot take the Laplacian of $S"))
    end
end

@operator +(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        (ActScalar, ActScalar) => Scalar
        (ActScalar, ActFormParams([i,d,s,n])) || (ActFormParams([i,d,s,n]), ActScalar) => S1 # commutativity
        (ActFormParams([i1,d1,s1,n1]), ActFormParams([i2,d2,s2,n2])) => begin
            if (i1 == i2) && (d1 == d2) && (s1 == s2) && (n1 == n2)
                Form{i1, d1, s1, n1}
            else
                throw(SortError("""
                    Can not add two forms of different dimensions/dualities/spaces:
                        $((i1,d1,s1)) and $((i2,d2,s2))
                    """))
            end
        end
        _ => error("Nay!")
    end
end

@operator -(S1, S2)::DECQuantity begin +(S1, S2) end

@operator *(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        (Scalar, Scalar) => Scalar
        (Scalar, ActFormParams([i,d,s,n])) || (ActFormParams([i,d,s,n]), Scalar) => Form{i,d,s,n}
        _ => throw(SortError("Cannot multiple $S1 and $S2"))
    end
end

@operator ∧(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        (ActFormParams([i1,d1,s1,n1]), ActFormParams([i2,d2,s2,n2])) => begin
            (d1 == d2) && (s1 == s2) && (n1 == n2) || throw(SortError("Can only take a wedge product of two forms of the same duality on the same space"))
            if i1 + i2 <= n1
                Form{i1 + i2, d1, s1, n1}
            else
                throw(SortError("Can only take a wedge product when the dimensions of the forms add to less than n, where n = $n1 is the dimension of the ambient space: tried to wedge product $i1 and $i2"))
            end
        end
    end
end

struct SortError <: Exception
    message::String
end

# struct WedgeDimError <: SortError end

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

# show methods
show_duality(ω::Form) = isdual(ω) ? "dual" : "primal"

function Base.show(io::IO, ω::Form)
    print(io, isdual(ω) ? "DualForm($(dim(ω))) on $(space(ω))" : "PrimalForm($(dim(ω))) on $(space(ω))")
end

Base.nameof(::typeof(-), s1, s2) = Symbol("$(as_sub(dim(s1)))-$(as_sub(dim(s2)))")

const SUBSCRIPT_DIGIT_0 = '₀'

as_sub(n::Int) = join(map(d -> SUBSCRIPT_DIGIT_0 + d, digits(n)))

function Base.nameof(::typeof(∧), s1::B1, s2::B2) where {S1,S2,B1<:BasicSymbolic{S1}, B2<:BasicSymbolic{S2}}
    Symbol("∧$(as_sub(dim(symtype(s1))))$(as_sub(dim(symtype(s2))))")
end

function Base.nameof(::typeof(∧), s1, s2)
    Symbol("∧$(as_sub(dim(s1)))$(as_sub(dim(s2)))")
end

Base.nameof(::typeof(d), s) = Symbol("d$(as_sub(dim(s)))")

function Base.nameof(::typeof(⋆), s)
    inv = isdual(s) ? "⁻¹" : ""
    Symbol("★$(as_sub(isdual(s) ? dim(space(s)) - dim(s) : dim(s)))$(inv)")
end

function SymbolicUtils.symtype(::Quantity, qty::Symbol, space::Symbol)
    @match qty begin
        :Scalar => Scalar
        :Form0 => PrimalForm{0, space, 1}
        :Form1 => PrimalForm{1, space, 1}
        :Form2 => PrimalForm{2, space, 1}
        :DualForm0 => DualForm{0, space, 1}
        :DualForm1 => DualForm{1, space, 1}
        :DualForm2 => DualForm{2, space, 1}
        _ => error("$qty")
    end
end

end
