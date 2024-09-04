using MLStyle
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic, FnType, Sym, symtype
import SymbolicUtils: promote_symtype

function promote_symtype(f::ComposedFunction, args)
    promote_symtype(f.outer, promote_symtype(f.inner, args))
end

""" DECQuantities in DiagrammaticEquations must be subtypes of Number to integrate with SymbolicUtils. An intermediary type, Quantity, makes it clearer that terms in the theory are "symbolic quantities" which behave like numbers. In the context of SymbolicUtils, a Number is any type that you can do arithmetic operations on.
"""
abstract type Quantity <: Number end
export Quantity

"""
Creates an operator `foo` with arguments which are types in a given Theory. This entails creating (1) a function which performs type construction and (2) a function which consumes BasicSymbolic variables and returns Terms.

```
@operator foo(S1, S2, ...)::Theory begin
    (body of function)
end     
```
builds
```
foo(::Type{S1}, ::Type{S2}, ...) where {S1<:ThDEC, S2<:ThDEC, ...}
    (body of function)
end
```
as well as
```
foo(S1::BasicSymbolic{T1}, S2::BasicSymbolic{T2}, ...) where {T1<:ThDEC, ...}
    s = foo(T1, T2, ...)
    SymbolicUtils.Term{s}(foo, [S1, S2, ...])
end
```

```
@operator Δ(s)::DECQuantity begin
    @match s begin
        ::Scalar => error("Invalid")
        ::VField => error("Invalid")
        ::Form => ⋆(d(⋆(d(s))))
    end
    # @rule ~x --> ⋆(d(⋆(d(s))))
end
```

# relationship between
#   - type-rewriting (via Metatheory)
#   - pattern-matching (via MLStyle, e.g. active pattners)
@operator Δ(s)::ThDEC begin
    @rule Δ(s::PrimalForm{0, X, 1}) --> ⋆(d(⋆(d(s))))
    @rule Δ(s::PrimalForm{1, X, 1}) --> ⋆(d(⋆(d(s))))
    _ => nothing
end
        

will create an additional method for Δ for operating on BasicSymbolic 
"""
macro operator(head, body)

    # parse body
    ph = @λ begin
        Expr(:call, foo, Expr(:(::), vars..., theory)) => (foo, vars, theory)
        Expr(:(::), Expr(:call, foo, vars...), theory) => (foo, vars, theory)
        _ => error("$head")
    end
    (f, types, Theory) = ph(head)

    sort_types = [:(::Type{$S}) for S in types]
    sort_constraints = [:($S<:$Theory) for S in types]
    
    # initialize the result
    result = quote end
    
    # DEFINE TYPE INFERENCE IN THE ThDEC SYSTEM
    
    push!(result.args, quote 
        function $f end; export $f 
    end)

    arity = length(sort_types)

    # we want to feed symtype the generics
    push!(result.args, quote
        function SymbolicUtils.promote_symtype(::typeof($f), $(sort_types...)) where {$(sort_constraints...)}
            $body
        end
        function SymbolicUtils.promote_symtype(::typeof($f), args::Vararg{Symbolic, $arity})
            promote_symtype($f, symtype.(args)...)
        end
    end)

    # CONSTRUCT THE FUNCTION ON BASIC SYMBOLICS

    # ...associate each var (S1) to a generic. this will be used in the
    # type constraint of the new function.
    generic_types = [(v, Symbol("T$k")) for (k,v) in enumerate(types)]

    # reassociate types with their BasicSymbolic Generic Types
    basicsym_bindings = map(generic_types) do (var, T)
        (var, :Symbolic)
    end

    # binding type bindings to the basicsymbolics
    bs_arg_exprs = map(basicsym_bindings) do (var, bs_gen)
        [:($var::$bs_gen)]
    end
    constraint_exprs = [:($T<:$Theory) for T in types]

    # Δ(x::BasicSymbolic(T1}) where T1<:DECQuantity
    # should be args subtype of symbolic, not basicsymbolic, not were-clause
    push!(result.args, quote
        @nospecialize
        function $f(args...)
            s = promote_symtype($f, args...)
            SymbolicUtils.Term{s}($f, [args...])
        end
        export $f
    end)

    return esc(result)
end
export @operator

function alias(x)
    error("$x has no aliases")
end

"""
Given a tuple of symbols ("aliases") and their canonical name (or "rep"), produces
for each alias typechecking and nameof methods which call those for their rep.
Example:
@alias (d₀, d₁) => d
"""
macro alias(body)
    (rep, aliases) = @match body begin
        Expr(:call, :(=>), Expr(:tuple, aliases...), rep) => (rep, aliases)
        _ => error("parse error")
    end
    result = quote end
    foreach(aliases) do alias
        push!(result.args,
            esc(quote
                function $alias(s...)
                    $rep(s...) 
                end
                export $alias
                Base.nameof(::typeof($alias), s) = Symbol("$alias")
            end))
    end
    result
end
export alias
