using ..Util.HashColor

using Base: Workqueue
using StructEquality
using ComponentArrays
using MLStyle
using Dtries
using SymbolicUtils
using SymbolicUtils: BasicSymbolic, Symbolic
using Reexport
using AssociatedTests
using Test

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

# DECVar{s}
struct OfSort{s} <: Number end
export OfSort

struct SortMetadata end

struct Equation{E}
    lhs::E
    rhs::E
end

function Base.:(==)(eq1::Equation, eq2::Equation)
    isequal(eq1.lhs, eq2.lhs) && isequal(eq1.rhs, eq2.rhs)
end

"""    Roe
"""
struct Roe
    working_path::Path
    vars::Dtry{Symbolic}
    eqs::Vector{Equation{Symbolic}}
end
export Roe

function Roe()
    Roe(Path([]), Dtry{Symbolic}(), Equation{Symbolic}[])
end

working_path(roe::Roe) = getfield(roe, :working_path)
vars(roe::Roe) = getfield(roe, :vars)
eqs(roe::Roe) = getfield(roe, :eqs)
export vars, eqs

@tests Roe begin
    roe = Roe()
    @test roe isa Roe
    @test working_path(roe) == Path([])
    @test vars(roe) == Dtries.Empty{Symbolic}()
    @test eqs(roe) == Equation{Symbolic}[]
end

function Base.getindex(roe::Roe, path::Path)::Symbolic
    path = working_path(roe) * path
    vars(roe)[path]
end

function Base.setindex!(roe::Roe, v::Symbolic, path::Path)
    setindex!(vars(roe), v, working_path(roe) * path)
end

@tests Tuple{getindex,setindex!} begin
    roe = Roe()
    @syms a

    roe[Path([:a, :b])] = a
    @test roe[Path([:a, :b])] === a
    roe[Path([:a, :c])] = a
    @test roe[Path([:a, :c])] === a

    @test_throws Exception (roe[Path([:a])] = a)
end

function fresh!(roe::Roe, name::Symbol, sort)
    v = SymbolicUtils.Sym{OfSort{sort}}(name)
    roe[Path([name])] = v
    v
end
export fresh!

@tests fresh! begin
    roe = Roe()
    x = fresh!(roe, :x, 1)

    @test x isa SymbolicUtils.Sym{OfSort{1}}
    @test roe[Path([:x])] === x
end

function Base.getproperty(roe::Roe, x::Symbol)
    p = working_path(roe) * Path([x])
    @match Dtries.lookup(vars(roe), p) begin
        Some(v) => v
        Nothing => Roe(p, vars(roe), eqs(roe))
    end
end

@tests getproperty begin
    roe = Roe()
    b = fresh!(roe.a, :b, 1)

    @test roe.a.b === b
end

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
        :($n = $(fresh!)($roe, $(QuoteNode(n)), $t))
    end
    esc(Expr(:block, stmts...))
end
export @vars

parse_var = @λ begin
    s::Symbol => (s, Number)
    Expr(:(::), name, type) => (name, type)
    err => error("$err is not a valid expression for a symbolic variable")
end

@tests var"@vars" begin
    roe = Roe()

    @vars roe a b

    @test roe.a === a
    @test roe.b === b

    @vars roe.x a b

    @test roe.x.a === a
    @test roe.x.b === b
end

function equate(roe::Roe, lhs::Symbolic, rhs::Symbolic)
    push!(eqs(roe), Equation{Symbolic}(lhs, rhs))
end

macro eq(roe, eq_expr)
    (lhs, rhs) = parse_eq(eq_expr)
    esc(quote
        $(equate)($roe, $lhs, $rhs)
    end)
end

parse_eq = @λ begin
    Expr(:call, :(==), lhs, rhs) => (lhs, rhs)
    err => error("$err is not a valid expression for an equation")
end

@tests var"@eq" begin
    roe = Roe()

    @vars roe a b

    @eq roe a == b

    @eq roe (a + b == b + a)

    @test eqs(roe) == [
        Equation{Symbolic}(a, b),
        Equation{Symbolic}(a + b, b + a)
    ]
end

function instantiate(f)
    roe = Roe()
    f(roe)
    roe
end
export instantiate
