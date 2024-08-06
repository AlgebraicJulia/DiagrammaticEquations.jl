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

  function TypedApplication(head::Function, sorts::Vector{Sort}) where Sort
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

Base.get(lookup::Dict{TA, Any}, key::TA) = lookup[key]
export get

###

struct Dtry{T} end

# DECVar{s}
struct AnyVar{s} <: Number end
export AnyVar

"""    Roe
"""
struct Roe{Sort}
  vars::ComponentArray{SymbolicUtils.BasicSymbolic{AnyVar{Sort}}}
  eqs::Vector{Tuple{Sort, Sort}}
  # 
  function Roe(Sort::DataType)
    new{Sort}(ComponentArray{SymbolicUtils.BasicSymbolic{AnyVar{Sort}}}(), [])
  end
end
export Roe

vars(roe::Roe) = roe.vars
eqs(roe::Roe) = roe.eqs
export vars, eqs

"""    @vars
Example: @vars roe u::Form(0)
"""
macro vars(roe, vars...)
  eval(vars_impl(roe, vars, __module__))
end
export @vars

export BasicSymbolic

# roe = Roe(DEC.ThDEC.Sort)
# @vars roe u::PrimalForm(0) v::PrimalForm(1) 

function vars_impl(roe, vars, __module__)
  roe = getfield(__module__, roe) 
  parsed_vars = parse_vars(vars)
  # Expr(:macrocall, Expr(:(::), :foo, :Scalar), Expr(:(::), :bar, Expr(:call, [:Form, 1])))
  kws = Expr(:macrocall, :@syms, map((n, t) -> Expr(:(::), n, t), parsed_vars))
  kws = map(parsed_vars) do (n, t)
    [n, 
      :(@syms $n::BasicSymbolic{AnyVar{$t}})]
  end
  return Expr(:call, :ComponentArray, Expr(:parameters, kws...,) $(vars(roe))) 
end

parse_vars = @λ begin
  t :: Tuple => parse_vars.(t)
  s :: Symbol => (s, Number)
  Expr(:(::), name, type) => (name, type)
  err => error("$err is not a valid expression for a symbolic variable")
end


