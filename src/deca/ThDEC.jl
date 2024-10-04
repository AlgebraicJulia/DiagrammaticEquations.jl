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

abstract type AbstractScalar <: DECQuantity end

struct InferredType <: DECQuantity end
export InferredType

struct Scalar <: AbstractScalar end
struct Parameter <: AbstractScalar end
struct Const <: AbstractScalar end
struct Literal <: AbstractScalar end
export Scalar, Parameter, Const, Literal

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

@active PatInferredType(T) begin
    if T <: InferredType
        Some(InferredType)
    end
end

@active PatInferredTypes(T) begin
    if any(S->S<:InferredType, T)
        Some(InferredType)
    end
end

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
    if T <: AbstractScalar
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
        PatInferredType(_) => InferredType
        PatFormParams([i,d,s,n]) => Form{i+1,d,s,n}
        _ => throw(OperatorError("take the exterior derivative", S))
    end
end

@alias (d₀, d₁) => d

@operator ★(S)::DECQuantity begin
    @match S begin
        PatInferredType(_) => InferredType
        PatFormParams([i,d,s,n]) => Form{n-i,d,s,n}
        _ => throw(OperatorError("take the hodge star", S))
    end
end

# TODO in orthodox Decapodes, these are type-specific.
@alias (★₀, ★₁, ★₂, ★₀⁻¹, ★₁⁻¹, ★₂⁻¹) => ★

@operator Δ(S)::DECQuantity begin
    @match S begin
        PatInferredType(_) => InferredType
        PatForm(_) => promote_symtype(★ ∘ d ∘ ★ ∘ d, S)
        _ => throw(OperatorError("take the Laplacian", S))
    end
    @rule Δ(~x::isForm0) => ★(d(★(d(~x))))
    @rule Δ(~x::isForm1) => ★(d(★(d(~x)))) + d(★(d(★(~x))))
    @rule Δ(~x::isForm2) => d(★(d(★(~x))))
end

@alias (Δ₀, Δ₁, Δ₂) => Δ

# TODO: Determine what we need to do for .+
@operator +(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        PatInferredTypes(_) => InferredType
        (PatScalar(_), PatScalar(_)) => Scalar
        (PatScalar(_), PatFormParams([i,d,s,n])) || (PatFormParams([i,d,s,n]), PatScalar(_)) => Form{i, d, s, n} # commutativity
        (PatFormParams([i1,d1,s1,n1]), PatFormParams([i2,d2,s2,n2])) => begin
            if (i1 == i2) && (d1 == d2) && (s1 == s2) && (n1 == n2)
                Form{i1, d1, s1, n1}
            else
                throw(OperatorError("sum", [S1, S2]))
            end
        end
        _ => throw(OperatorError("add", [S1, S2]))
    end
end

@operator -(S1, S2)::DECQuantity begin
    promote_symtype(+, S1, S2)
end

@operator *(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        PatInferredTypes(_) => InferredType
        (PatScalar(_), PatScalar(_)) => Scalar
        (PatScalar(_), PatFormParams([i,d,s,n])) || (PatFormParams([i,d,s,n]), PatScalar(_)) => Form{i,d,s,n}
        _ => throw(OperatorError("multiply", [S1, S2]))
    end
end

@operator ∧(S1, S2)::DECQuantity begin
    @match (S1, S2) begin
        PatInferredTypes(_) => InferredType
        (PatFormParams([i1,d1,s1,n1]), PatFormParams([i2,d2,s2,n2])) => begin
            (d1 == d2) && (s1 == s2) && (n1 == n2) || throw(OperatorError("take the wedge product", [S1, S2]))
            if i1 + i2 <= n1
                Form{i1 + i2, d1, s1, n1}
            else
                throw(OperatorError("take the wedge product", [S1, S2], "The dimensions of the form are bounded by $n1"))
            end
        end
        _ => throw(OperatorError("take the wedge product", [S1, S2]))
    end
end

abstract type SortError <: Exception end

Base.nameof(s::Union{Literal,Type{Literal}}) = :Literal
Base.nameof(s::Union{Const, Type{Const}}) = :Constant
Base.nameof(s::Union{Parameter, Type{Parameter}}) = :Parameter
Base.nameof(s::Union{Scalar, Type{Scalar}}) = :Scalar

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
  if isdual(ω)
    print(io, "DualForm($(dim(ω))) on $(space(ω))")
  else
    print(io, "PrimalForm($(dim(ω))) on $(space(ω))")
  end
end

sub_dim(s) = as_sub(dim(s))

Base.nameof(::typeof(-), s1, s2) = Symbol("$(sub_dim(s1))-$(sub_dim(s2))")

const SUBSCRIPT_DIGIT_0 = '₀'

as_sub(n::Int) = join(map(d -> SUBSCRIPT_DIGIT_0 + d, digits(n)))


function Base.nameof(::typeof(∧), s1::B1, s2::B2) where {S1,S2,B1<:BasicSymbolic{S1}, B2<:BasicSymbolic{S2}}
    Symbol("∧$(sub_dim(symtype(s1)))$(sub_dim(symtype(s2)))")
end

function Base.nameof(::typeof(∧), s1, s2)
    Symbol("∧$(sub_dim(s1))$(sub_dim(s2))")
end

Base.nameof(::typeof(∂ₜ), s) = Symbol("∂ₜ($(nameof(s)))")

Base.nameof(::typeof(d), s) = Symbol("d$(sub_dim(s))")

# Base.nameof(::typeof(Δ), s) = Symbol("Δ$(sub_dim(s))")
Base.nameof(::typeof(Δ), s) = :Δ

function Base.nameof(::typeof(★), s)
    inv = isdual(s) ? "⁻¹" : ""
    hdg_dim = isdual(s) ? spacedim(s) - dim(s) : dim(s)
    Symbol("★$(as_sub(hdg_dim))$(inv)")
end

# TODO: Check that form type is no larger than the ambient dimension
function SymbolicUtils.symtype(::Type{<:Quantity}, qty::Symbol, space::Symbol, dim::Int = 2)
    @match qty begin
        :Scalar => Scalar
        :Constant => Const
        :Parameter => Parameter
        :Literal => Literal
        :Form0 => PrimalForm{0, space, dim}
        :Form1 => PrimalForm{1, space, dim}
        :Form2 => PrimalForm{2, space, dim}
        :DualForm0 => DualForm{0, space, dim}
        :DualForm1 => DualForm{1, space, dim}
        :DualForm2 => DualForm{2, space, dim}
        :Infer => InferredType
        _ => error("Received $qty")
    end
end

struct OperatorError <: SortError
    verb::String
    sorts::Vector{DataType}
    othermsg::String
    function OperatorError(verb::String, sorts::Vector{DataType}, othermsg::String="")
        new(verb, sorts, othermsg)
    end
    function OperatorError(verb::String, sort::DataType, othermsg::String="")
        new(verb, [sort], othermsg)
    end
end
export OperatorError

Base.showerror(io::IO, e::OperatorError) = print(io, "Cannot take the $(e.verb) of $(join(e.sorts, " and ")). $(e.othermsg)")

end
