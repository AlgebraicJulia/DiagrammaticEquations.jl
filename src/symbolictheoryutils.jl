using MLStyle
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic, FnType, Sym, symtype
import SymbolicUtils: promote_symtype

function promote_symtype(f::ComposedFunction, args)
    promote_symtype(f.outer, promote_symtype(f.inner, args))
end

@active PatBlock(e) begin
    @match e begin
        Expr(:macrocall, name, args...) && if name == Symbol("@", "match") end => Some(e)
        Expr(:block, args...) => Some(e)
        Expr(:let, args...) => Some(e)
        _ => nothing
    end
end
export PatBlock

@active PatRule(e) begin
    @match e begin
        Expr(:macrocall, head, args...) && if head == Symbol("@", "rule") end => Some(e)
        _ => nothing
    end
end
export PatRule

""" DECQuantities in DiagrammaticEquations must be subtypes of Number to integrate with SymbolicUtils. An intermediary type, Quantity, makes it clearer that terms in the theory are "symbolic quantities" which behave like numbers. In the context of SymbolicUtils, a Number is any type that you can do arithmetic operations on.
"""
abstract type Quantity <: Number end
export Quantity

"""
Creates an operator `foo` with arguments which are types in a given Theory. This entails creating (1) a function which performs type construction and (2) a function which consumes BasicSymbolic variables and returns Terms.

```
@operator foo(S1, S2, ...)::Theory begin
    (body of function)
    (@rule expr1)
    ...
    (@rule exprN)
end     
```
builds
```
promote_symtype(::typeof{f}, ::Type{S1}, ::Type{S2}, ...) where {S1<:DECQuantity, S2<:DECQuantity, ...}
    (body of function)
end
```
as well as
```
foo(S1, S2, ...) where {T1<:ThDEC, ...}
    s = promote_symtype(f, S1, S2, ...)
    SymbolicUtils.Term{s}(foo, [S1, S2, ...])
end
```

Example:
```
@operator Δ(s)::DECQuantity begin
    @match s begin
        ::Scalar => error("Invalid")
        ::VField => error("Invalid")
        ::Form => ⋆(d(⋆(d(s))))
    end
    @rule ~s --> ⋆(d(⋆(d(~s))))
end
```
"""
macro operator(head, body)

    # parse body
    ph = @λ begin
        Expr(:call, foo, Expr(:(::), vars..., theory)) => (foo, vars, theory)
        Expr(:(::), Expr(:call, foo, vars...), theory) => (foo, vars, theory)
        _ => error("$head")
    end
    (f, types, Theory) = ph(head)

    # Passing types to functions requires that we type the signature with ::Type{T}. 
    # This means that the user would have to write `my_op(::Type{T1}, ::Type{T2}, ...)`
    # As a convenience to the user, we allow them to specify the signature using just the types themselves:
    # `my_op(T1, T2, ...)`
    sort_types = [:(::Type{$S}) for S in types]
    sort_constraints = [:($S<:$Theory) for S in types]
    arity = length(sort_types)

    # Parse the body for @rule calls. 
    block, rulecalls = @match Base.remove_linenums!(body) begin
        Expr(:block, block, rules...) => (block, rules)
        s => nothing
    end
    
    # initialize the result
    result = quote end
    
    # construct the function on basic symbolics
    push!(result.args, quote
        @nospecialize
        function $f(args...)
            s = promote_symtype($f, args...)
            SymbolicUtils.Term{s}($f, [args...])
        end
        export $f
    end)

    # we want to feed symtype the generics
    push!(result.args, quote
        function SymbolicUtils.promote_symtype(::typeof($f), $(sort_types...)) where {$(sort_constraints...)}
            $block
        end
        function SymbolicUtils.promote_symtype(::typeof($f), args::Vararg{Symbolic, $arity})
            promote_symtype($f, symtype.(args)...)
        end
    end)

    push!(result.args, Expr(:tuple, rulecalls...))

    return esc(result)
end
export @operator

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
export @alias

function alias(x)
    error("$x has no aliases")
end
export alias

