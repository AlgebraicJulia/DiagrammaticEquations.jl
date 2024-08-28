using MLStyle
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic, FnType, Sym

abstract type Quantity <: Number end
export Quantity

"""
Registers a new function

```
@register foo(S1, S2, ...)::ThDEC begin
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
@register Δ(s::ThDEC) begin
    @match s begin
        ::Scalar => error("Invalid")
        ::VField => error("Invalid")
        ::Form => ⋆(d(⋆(d(s))))
    end
end
```

Δ(S1, S2) begin
    @match (S1, S2)
        

will create an additional method for Δ for operating on BasicSymbolic 
"""
macro register(head, body)

    # parse body
    ph = @λ begin
        Expr(:call, foo, Expr(:(::), vars..., theory)) => (foo, vars, theory)
        Expr(:(::), Expr(:call, foo, vars...), theory) => (foo, vars, theory)
        _ => error("$head")
    end
    (f, vars, Theory) = ph(head)

    symbolic_args = [:(::Type{$S}) for S in vars]
    symbolic_constraints = [:($S<:$Theory) for S in vars]
    
    # initialize the result
    result = quote end
    
    # DEFINE TYPE INFERENCE IN THE ThDEC SYSTEM
    
    # TODO this just accepts whatever the body is
    push!(result.args,
          esc(quote
                function $f($(symbolic_args...)) where {$(symbolic_constraints...)}
                    $body
              end
          end))

    # CONSTRUCT THE FUNCTION ON BASIC SYMBOLICS

    # ...associate each var (S1) to a generic. this will be used in the
    # type constraint of the new function.
    generic_vars = [(v, Symbol("T$k")) for (k,v) in enumerate(vars)]

    # reassociate vars with their BasicSymbolic Generic Types
    basicsym_bindings = map(generic_vars) do (var, T)
        (var, :(BasicSymbolic{$T}))
    end

    # binding type bindings to the basicsymbolics
    basicsym_args = [:($var::$basicsym_generic) for (var, basicsym_generic) in basicsym_bindings]

    # build constraints
    constraints_expr = [:($T<:$Theory) for T in getindex.(generic_vars, 2)]
 
    push!(result.args,
          esc(quote
              @nospecialize
              function $f($(basicsym_args...)) where {$(constraints_expr...)}
                  s = $f($(getindex.(generic_vars, 2)...))
                  SymbolicUtils.Term{s}($f ,[$(getindex.(basicsym_bindings, 1)...)])
              end
              export $f
          end))

    return result
end
export @register

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

macro see(body)
    ph = @λ begin
        Expr(:(=), Expr(:where, Expr(:call, foo, typebindings), params...), 
                    Expr(:block, body...)) => (foo, ph(typebindings), params, body)
        Expr(:(::), vars...) => ph.(vars)
        Expr(:curly, :Type, Expr(:<:, Expr(:curly, type, params...))) => (type, params)
        s => s
    end
    ph(body)
    quote
        $foo(arg, s1::B1, s2::B1) where {S1,S2,B1<:BasicSymbolic{S1},B2<:BasicSymbolic{S2}}

    end
end

@see dim(::Type{<:Form{i,d,s,n}}) where {i,d,s,n} = i

function Base.nameof(::typeof(∧), s1::B1, s2::B2) where {S1,S2,B1<:BasicSymbolic{S1}, B2<:BasicSymbolic{S2}}
    Symbol("∧$(as_sub(dim(symtype(s1))))$(as_sub(dim(symtype(s2))))")
end


Expr(:=,
     Expr(:where
          [Expr(:call
                foo,
                Expr(:(::), e...)),
           params...]),
     Expr(:block, body...))
