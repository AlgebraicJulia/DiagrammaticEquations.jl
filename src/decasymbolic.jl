module SymbolicUtilsInterop

using ..DiagrammaticEquations: AbstractDecapode
import ..DiagrammaticEquations: eval_eq!, SummationDecapode
using ..ThDEC
import ..ThDEC: Sort, dim, isdual
using ..decapodes

using MLStyle
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic, FnType, Sym

# ##########################
# DECType
#
# Type necessary for symbolic utils
# ##########################

# define DECType as a Number. Necessary for SymbolicUtils
abstract type DECType <: Number end

"""
i: dimension: 0,1,2, etc.
d: duality: true = dual, false = primal
s: name of the space (a symbol)
n: dimension of the space
"""
struct FormT{i,d,s,n} <: DECType end
export FormT

struct VFieldT{d,s,n} <: DECType end
export VFieldT

dim(::Type{<:FormT{i,d,s,n}}) where {i,d,s,n} = d
isdual(::Type{FormT{i,d,s,n}}) where {i,d,s,n} = d

# convenience functions
const PrimalFormT{i,s,n} = FormT{i,false,s,n}
export PrimalFormT

const DualFormT{i,s,n} = FormT{i,true,s,n}
export DualFormT

const PrimalVFT{s,n} = VFieldT{false,s,n}
export PrimalVFT

const DualVFT{s,n} = VFieldT{true,s,n}
export DualVFT

"""
converts ThDEC Sorts into DECType
"""
function Sort end

Sort(::Type{<:Real}) = Scalar()
Sort(::Real) = Scalar()

function Sort(::Type{FormT{i,d,s,n}}) where {i,d,s,n}
    Form(i, d, Space(s, n))
end

function Sort(::Type{VFieldT{d,s,n}}) where {d,s,n}
    VField(d, Space(s, n))
end

Sort(::BasicSymbolic{T}) where {T} = Sort(T)

"""
converts ThDEC Sorts into DecaSymbolic types
"""
Number(s::Scalar) = Real

Number(f::Form) = FormT{dim(f),isdual(f), nameof(space(f)), dim(space(f))}

Number(v::VField) = VFieldT{isdual(v), nameof(space(v)), dim(space(v))}

# HERE WE DEFINE THE SYMBOLICUTILS

# for every unary operator in our theory, take a BasicSymbolic type, convert its type parameter to a Sort in our theory, and return a term
unop_dec = [:∂ₜ, :d, :d₀, :d₁
            , :⋆, :⋆₀, :⋆₁, :⋆₂, :⋆₀⁻¹, :⋆₁⁻¹, :⋆₂⁻¹
            , :♯, :♭, :-]

for unop in unop_dec
    @eval begin
        @nospecialize
        function ThDEC.$unop(
            v::BasicSymbolic{T}
        ) where {T<:DECType}
            s = ThDEC.$unop(Sort(T))
            SymbolicUtils.Term{Number(s)}(ThDEC.$unop, [v])
        end
    end
end

# BasicSymbolic{FnType{Tuple{PrimalFormT{0}}}, PrimalFormT{0}}

binop_dec = [:+, :-, :*, :∧, :^]
export +,-,*,∧,^

for binop in binop_dec
    @eval begin
        @nospecialize
        function ThDEC.$binop(
            v::BasicSymbolic{T1},
            w::BasicSymbolic{T2}
        ) where {T1<:DECType,T2<:DECType}
            s = ThDEC.$binop(Sort(T1), Sort(T2))
            SymbolicUtils.Term{Number(s)}(ThDEC.$binop, [v, w])
        end

        @nospecialize
        function ThDEC.$binop(
            v::BasicSymbolic{T1},
            w::BasicSymbolic{T2}
        ) where {T1<:DECType,T2<:Real}
            s = ThDEC.$binop(Sort(T1), Sort(T2))
            SymbolicUtils.Term{Number(s)}(ThDEC.$binop, [v, w])
        end

        @nospecialize
        function ThDEC.$binop(
            v::BasicSymbolic{T1},
            w::BasicSymbolic{T2}
        ) where {T1<:Real,T2<:DECType}
            s = ThDEC.$binop(Sort(T1), Sort(T2))
            SymbolicUtils.Term{Number(s)}(ThDEC.$binop, [v, w])
        end
    end
end

# name collision with decapodes.Equation
struct DecaEquation{E}
    lhs::E
    rhs::E
end
export DecaEquation

Base.show(io::IO, e::DecaEquation) = begin
    print(io, e.lhs)
    print(io, " == ")
    print(io, e.rhs)
end

# a struct carry the symbolic variables and their equations
struct DecaSymbolic
    vars::Vector{Symbolic}
    equations::Vector{DecaEquation{Symbolic}}
end
export DecaSymbolic

Base.show(io::IO, d::DecaSymbolic) = begin
    println(io, "DecaSymbolic(")
    println(io, "  Variables: [$(join(d.vars, ", "))]")
    println(io, "  Equations: [")
    eqns = map(d.equations) do op
      "    $(op)"
    end
    println(io, "$(join(eqns,",\n"))])")
  end

# BasicSymbolic -> DecaExpr
function decapodes.Term(t::SymbolicUtils.BasicSymbolic)
    if SymbolicUtils.issym(t)
        decapodes.Var(nameof(t))
    else
        op = SymbolicUtils.head(t)
        args = SymbolicUtils.arguments(t)
        termargs = Term.(args)
        sorts = ThDEC.Sort.(args)
        if op == +
            decapodes.Plus(termargs)
        elseif op == *
            decapodes.Mult(termargs)
        elseif op == ThDEC.∂ₜ
            decapodes.Tan(only(termargs))
        elseif length(args) == 1
            decapodes.App1(nameof(op, sorts...), termargs...)
        elseif length(args) == 2
            decapodes.App2(nameof(op, sorts...), termargs...)
        else
            error("was unable to convert $t into a Term")
        end
    end
end

decapodes.Term(x::Real) = decapodes.Lit(Symbol(x))

function decapodes.DecaExpr(d::DecaSymbolic)
    context = map(d.vars) do var
        decapodes.Judgement(nameof(var), nameof(Sort(var)), :X)
    end
    equations = map(d.equations) do eq
        decapodes.Eq(decapodes.Term(eq.lhs), decapodes.Term(eq.rhs))
    end
    decapodes.DecaExpr(context, equations)
end

"""
Retrieve the SymbolicUtils expression of a DecaExpr term `t` from a context of variables in ThDEC

Example:
```
  a = @syms a::Real
  context = Dict(:a => Scalar(), :u => PrimalForm(0))
  SymbolicUtils.BasicSymbolic(context, Term(a))
```
"""
function SymbolicUtils.BasicSymbolic(context::Dict{Symbol,Sort}, t::decapodes.Term)
    # user must import symbols into scope
    ! = (f -> getfield(Main, f))
    @match t begin
        Var(name) => SymbolicUtils.Sym{Number(context[name])}(name)
        Lit(v) => Meta.parse(string(v))
        # see heat_eq test: eqs had AppCirc1, but this returns
        # App1(f, App1(...)
        AppCirc1(fs, arg) => foldr(
            # panics with constants like :k
            # see test/language.jl
            (f, x) -> (!(f))(x),
            fs;
            init=BasicSymbolic(context, arg)
        )
        # getfield(Main, 
        App1(f, x) => (!(f))(BasicSymbolic(context, x))
        App2(f, x, y) => (!(f))(BasicSymbolic(context, x), BasicSymbolic(context, y))
        Plus(xs) => +(BasicSymbolic.(Ref(context), xs)...)
        Mult(xs) => *(BasicSymbolic.(Ref(context), xs)...)
        Tan(x) => ThDEC.∂ₜ(BasicSymbolic(context, x))
    end
end

function DecaSymbolic(lookup::SpaceLookup, d::decapodes.DecaExpr)
    # associates each var to its sort...
    context = map(d.context) do j
        j.var => ThDEC.fromexpr(lookup, j.dim, Sort)
    end
    # ... which we then produce a vector of symbolic vars
    vars = map(context) do (v, s)
        SymbolicUtils.Sym{Number(s)}(v)
    end
    context = Dict{Symbol,Sort}(context)
    eqs = map(d.equations) do eq
        DecaEquation{Symbolic}(BasicSymbolic.(Ref(context), [eq.lhs, eq.rhs])...)
    end
    DecaSymbolic(vars, eqs)
end

function eval_eq!(eq::DecaEquation, d::AbstractDecapode, syms::Dict{Symbol, Int}, deletions::Vector{Int})
    eval_eq!(Equation(Term(eq.lhs), Term(eq.rhs)), d, syms, deletions)
end

"""    function SummationDecapode(e::DecaSymbolic) """
function SummationDecapode(e::DecaSymbolic)
    d = SummationDecapode{Any, Any, Symbol}()
    symbol_table = Dict{Symbol, Int}()

    foreach(e.vars) do var
        # convert Sort(var)::PrimalForm0 --> :Form0
        var_id = add_part!(d, :Var, name=var.name, type=nameof(Sort(var)))
        symbol_table[var.name] = var_id
    end

    deletions = Vector{Int}()
    foreach(e.equations) do eq
        eval_eq!(eq, d, symbol_table, deletions)
    end
    rem_parts!(d, :Var, sort(deletions))

    recognize_types(d)

    fill_names!(d)
    d[:name] = normalize_unicode.(d[:name])
    make_sum_mult_unique!(d)
    return d
end

"""
Registers a new function

```
@register Δ(s::Sort) begin
    @match s begin
        ::Scalar => error("Invalid")
        ::VField => error("Invalid")
        ::Form => ⋆(d(⋆(d(s))))
    end
end
```

will create an additional method for Δ for operating on BasicSymbolic 
"""
macro register(head, body)
    # parse head
    parsehead = @λ begin
        Expr(:call, f, types...) => (f, parsehead.(types))
        Expr(:(::), var, type) => (var, type)
        s => s
    end
    (f, args) = parsehead(head)
    matchargs = [:($(x[1])::$(x[2])) for x in args]

    result = quote end
    push!(result.args,
          esc(quote
              function $f($(matchargs...))
                  $body
              end
          end))

    # e.g., given [(:x, :Scalar), (:ω, :Form)]...
    vs = enumerate(unique(getindex.(args, 2)))
    theargs =
    Dict{Symbol,Symbol}(
        [v => Symbol("T$k") for (k,v) in vs]
    )
    # ...[(Scalar=>:T1, :Form=>:T2)]

    # reassociate vars with their BasicSymbolic Generic Types
    binding = map(args) do (var, type)
        (var, :(BasicSymbolic{$(theargs[type])}))
    end
    newargs = [:($(x[1])::$(x[2])) for x in binding]
    constraints = [:($T<:DECType) for T in values(theargs)]
    innerargs = [:(Sort($T)) for T in values(theargs)]

    push!(result.args,
          quote
              @nospecialize
              function $(esc(f))($(newargs...)) where $(constraints...)
                  s = $(esc(f))($(innerargs...))
                  SymbolicUtils.Term{Number(s)}($(esc(f)), [$(getindex.(binding, 1)...)])
              end
          end)

    return result
end
export @register

end
