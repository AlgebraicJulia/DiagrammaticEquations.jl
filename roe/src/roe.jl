using ..Util.HashColor

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
struct AnyVar{s} <: Number end
export AnyVar

struct Equation{E}
    lhs::E
    rhs::E
end

"""    Roe
"""
struct Roe{E}
    vars::Dtry{E}
    eqs::Vector{Equation{E}}
    #
    function Roe{E}(Sort::DataType)
        new{Sort}(Dtry{E}(), Equation{E}[])
    end
end
export Roe

vars(roe::Roe) = roe.vars
eqs(roe::Roe) = roe.eqs
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
