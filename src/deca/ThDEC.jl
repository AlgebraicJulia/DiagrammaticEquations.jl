module ThDEC

using ..DiagrammaticEquations: @operator, @alias, Quantity
import ..DiagrammaticEquations: rules

using MLStyle
using StructEquality
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic, FnType, Sym, Term
import SymbolicUtils: symtype, promote_symtype

import Base: +, -, *
import Catlab: Δ, ∧

# ##########################
# DECQuantity
#
# Type necessary for symbolic utils
# ##########################

abstract type DECQuantity <: Quantity end
export DECQuantity

# this ensures symtype doesn't recurse endlessly
SymbolicUtils.symtype(::Type{S}) where S<:DECQuantity = S

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

Base.nameof(u::Type{<:PrimalForm}) = Symbol("Form"*"$(dim(u))")
Base.nameof(u::Type{<:DualForm}) = Symbol("DualForm"*"$(dim(u))")

# ACTIVE PATTERNS

@active PatForm(T) begin
    if T <: Form
        Some(T)
    end
end
export PatForm

@active PatFormParams(T) begin
    if T <: Form
        Some([T.parameters...])
    end
end
export PatFormParams

@active PatFormDim(T) begin
    if T <: Form
        Some(dim(T))
    end
end
export PatFormDim

@active PatScalar(T) begin
    if T <: Scalar
        Some(T)
    end
end
export PatScalar

@active PatVFParams(T) begin
    if T <: VField
        Some([T.parameters...])
    end
end
export PatVFParams

isDualForm(x) = @match symtype(x) begin
    PatFormParams([_,d,_,_]) => d
    _ => false
end

# TODO parameterize?
isForm0(x) = @match symtype(x) begin
    PatFormParams([0,_,_,_]) => true
    _ => false
end

isForm1(x) = @match symtype(x) begin
    PatFormParams([1,_,_,_]) => true
    _ => false
end

isForm2(x) = @match symtype(x) begin
    PatFormParams([2,_,_,_]) => true
    _ => false
end

export isDualForm, isForm0, isForm1, isForm2

# ###############################
# OPERATORS
# ###############################

@operator -(S)::DECQuantity begin S end

@operator ∂ₜ(S)::DECQuantity begin S end

@operator d(S)::DECQuantity begin
    @match S begin
        PatFormParams([i,d,s,n]) => Form{i+1,d,s,n}
        _ => throw(ExteriorDerivativeError(S))
    end
end

@alias (d₀, d₁) => d

@operator ★(S)::DECQuantity begin
    @match S begin
        PatFormParams([i,d,s,n]) => Form{n-i,d,s,n}
        _ => throw(HodgeStarError(S))
    end
end

@alias (★₀, ★₁, ★₂, ★₀⁻¹, ★₁⁻¹, ★₂⁻¹) => ★

@operator Δ(S)::DECQuantity begin
    @match S begin
        PatForm(_) => promote_symtype(★ ∘ d ∘ ★ ∘ d, S)
        _ => throw(LaplacianError(S))
    end
    @rule Δ(~x::isForm0) => ★(d(★(d(~x))))
    @rule Δ(~x::isForm1) => ★(d(★(d(~x)))) + d(★(d(★(~x))))
end

@alias (Δ₀, Δ₁, Δ₂) => Δ

@operator +(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        (PatScalar(_), PatScalar(_)) => Scalar
        (PatScalar(_), PatFormParams([i,d,s,n])) || (PatFormParams([i,d,s,n]), PatScalar(_)) => S1 # commutativity
        (PatFormParams([i1,d1,s1,n1]), PatFormParams([i2,d2,s2,n2])) => begin
            if (i1 == i2) && (d1 == d2) && (s1 == s2) && (n1 == n2)
                Form{i1, d1, s1, n1}
            else
                throw(AdditionDimensionalError(S1, S2))
            end
        end
        _ => throw(BinaryOpError("add", S1, S2))
    end
end

@operator -(S1, S2)::DECQuantity begin
    promote_symtype(+, S1, S2)
end

@operator *(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        (PatScalar(_), PatScalar(_)) => Scalar
        (PatScalar(_), PatFormParams([i,d,s,n])) || (PatFormParams([i,d,s,n]), PatScalar(_)) => Form{i,d,s,n}
        _ => throw(BinaryOpError("multiply", S1, S2))
    end
end

@operator ∧(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        (PatFormParams([i1,d1,s1,n1]), PatFormParams([i2,d2,s2,n2])) => begin
            (d1 == d2) && (s1 == s2) && (n1 == n2) || throw(WedgeOpError(S1, S2))
            if i1 + i2 <= n1
                Form{i1 + i2, d1, s1, n1}
            else
                throw(WedgeDimError(S1, S2))
            end
        end
        _ => throw(BinaryOpError("take the wedge product of", S1, S2))
    end
end

abstract type SortError <: Exception end

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

Base.nameof(::typeof(Δ), s) = :Δ

function Base.nameof(::typeof(★), s)
    inv = isdual(s) ? "⁻¹" : ""
    Symbol("★$(as_sub(isdual(s) ? dim(space(s)) - dim(s) : dim(s)))$(inv)")
end

function SymbolicUtils.symtype(::Type{<:Quantity}, qty::Symbol, space::Symbol)
    @match qty begin
        :Scalar || :Constant => Scalar
        :Form0 => PrimalForm{0, space, 1}
        :Form1 => PrimalForm{1, space, 1}
        :Form2 => PrimalForm{2, space, 1}
        :DualForm0 => DualForm{0, space, 1}
        :DualForm1 => DualForm{1, space, 1}
        :DualForm2 => DualForm{2, space, 1}
        _ => error("Received $qty")
    end
end

struct ExteriorDerivativeError <: SortError
    sort::DataType
end

Base.showerror(io::IO, e::ExteriorDerivativeError) = print(io, "Cannot apply the exterior derivative to $(e.sort)")

struct HodgeStarError <: SortError
    sort::DataType
end

Base.showerror(io::IO, e::HodgeStarError) = print(io, "Cannot take the hodge star of $(e.sort)")

struct LaplacianError <: SortError
    sort::DataType
end

Base.showerror(io::IO, e::LaplacianError) = print(io, "Cannot take the Laplacian of $(e.sort)")

struct AdditionDimensionalError <: SortError
    sort1::DataType
    sort2::DataType
end

Base.showerror(io::IO, e::AdditionDimensionalError) = print(io, """
                    Can not add two forms of different dimensions/dualities/spaces:
                    $(e.sort1) and $(e.sort2)
                        """)

struct BinaryOpError <: SortError
    verb::String
    sort1::DataType
    sort2::DataType
end

Base.showerror(io::IO, e::BinaryOpError) = print(io, "Cannot $(e.verb) $(e.sort1) and $(e.sort2)")

struct WedgeOpError <: SortError
    sort1::DataType
    sort2::DataType
end

Base.showerror(io::IO, e::WedgeOpError) = print(io, "Can only take a wedge product of two forms of the same duality on the same space. Received $(e.sort1) and $(e.sort2)")

struct WedgeOpDimError <: SortError
    sort1::DataType
    sort2::DataType
end

Base.showerror(io::IO, e::WedgeOpDimError) = print(io, "Can only take a wedge product when the dimensions of the forms add to less than n, where n = $(e.sort.dim) is the dimension of the ambient space: tried to wedge product $(e.sort1) and $(e.sort2)")

end
