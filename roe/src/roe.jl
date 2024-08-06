using ..Util.HashColor
using ..Util.Dtrys

using StructEquality
using ComponentArrays
using MLStyle
using SymbolicUtils
using SymbolicUtils: BasicSymbolic
using Reexport

"""
Sorts in each theory are subtypes of this abstract type.
"""
abstract type AbstractSort end
export AbstractSort

"""    TypedApplication

Struct containing a Function and the vector of Sorts it requires.
"""
@struct_hash_equal struct TypedApplication{Sort<:AbstractSort}
    head::Function
    sorts::Vector{Sort}

    function TypedApplication(head::Function, sorts::Vector{Sort}) where {Sort}
        new{Sort}(head, sorts)
    end
end
export TypedApplication

const TA = TypedApplication
export TA

Base.show(io::IO, ta::TA) = print(io, Expr(:call, nameof(ta.head), ta.sorts...))

struct SortError <: Exception
    message::String
end
export SortError

Base.get(lookup::Dict{TA,Any}, key::TA) = lookup[key]
export get

###

struct Dtry{T} end

# DECVar{s}
struct OfSort{s} <: Number end
export OfSort

struct Equation{E}
    lhs::E
    rhs::E
end

push!([Int[]], String[]) # error
push!(Vector[Int[]], String[]) # fine

"""    Roe
"""
struct Roe
    namespace::DtryVar
    vars::Dtry{Symbolic}
    eqs::Vector{Equation{Symbolic}}
end
export Roe

function Roe()
    Roe(Dtrys.■, Dtry{Symbolic}(), Equation{Symbolic}[])
end

function Base.getindex(roe::Roe, name::Symbol)::Union{Symbolic,Roe}
    path = namespace(roe)[name]
    if haskey(vars(roe), path)
        vars(roe)[path]
    else
        Roe(path, vars(roe), eqs(roe))
    end
end

function Base.getproperty(roe::Roe, name::Symbol)
    getindex(roe, name)
end

function Base.setindex!(roe::Roe, v::Symbolic, name::Symbol)
end

function Base.setproperty!(roe::Roe, v::Symbolic, name::Symbol)
end

function fresh!(roe::Roe, name::Symbol, sort)
    v = Sym{OfSort{sort}}(name)
    roe[name] = v
    v
end

namespace(roe::Roe) = getfield(roe, :namespace)
vars(roe::Roe) = getfield(roe, :vars)
eqs(roe::Roe) = getfield(roe, :eqs)
export vars, eqs

"""    @vars
Example: @vars roe u::Form(0)

```julia
@vars roe u::Form(0)

->

u = fresh!(roe, :u, Form(0))
```
"""
macro vars(roe, vars...)
    vars = parse_var.(vars)
    stmts = map(vars) do (n, t)
        :($n = fresh!($roe, $(QuoteNode(n)), $t))
    end
    Expr(:block, stmts...)
end
export @vars

parse_var = @λ begin
    s::Symbol => (s, Number)
    Expr(:(::), name, type) => (name, type)
    err => error("$err is not a valid expression for a symbolic variable")
end
