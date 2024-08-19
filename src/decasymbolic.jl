module SymbolicUtilsInterop

using ..ThDEC
using MLStyle
import ..ThDEC: Sort, dim, isdual
using ..decapodes
using SymbolicUtils

using SymbolicUtils: Symbolic, BasicSymbolic

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

# for every unary operator in our theory, take a BasicSymbolic type, convert its type parameter to a Sort in our theory, and return a term
unop_dec = [:∂ₜ, :d, :★, :♯, :♭, :-]
for unop in unop_dec
    @eval begin
        @nospecialize
        function ThDEC.$unop(
            v::BasicSymbolic{T}
        ) where {T<:DECType}
            # convert the DECType to ThDEC to type check
            s = ThDEC.$unop(Sort(T))
            # the resulting type is converted back to DECType
            # the resulting term has the operation has its head and `v` as its args.
            SymbolicUtils.Term{Number(s)}(ThDEC.$unop, [v])
        end
    end
end

binop_dec = [:+, :-, :*, :∧]
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
        export $binop

        @nospecialize
        function ThDEC.$binop(
            v::BasicSymbolic{T1},
            w::BasicSymbolic{T2}
        ) where {T1<:DECType,T2<:Real}
            s = ThDEC.$binop(Sort(T1), Sort(T2))
            SymbolicUtils.Term{Number(s)}(ThDEC.$binop, [v, w])
        end
        export $binop

        @nospecialize
        function ThDEC.$binop(
            v::BasicSymbolic{T1},
            w::BasicSymbolic{T2}
        ) where {T1<:Real,T2<:DECType}
            s = ThDEC.$binop(Sort(T1), Sort(T2))
            SymbolicUtils.Term{Number(s)}(ThDEC.$binop, [v, w])
        end
        export $binop
    end
end

# name collision with decapodes.Equation
struct DecaEquation{E}
    lhs::E
    rhs::E
end
export DecaEquation

# a struct carry the symbolic variables and their equations
struct DecaSymbolic
    vars::Vector{Symbolic}
    equations::Vector{DecaEquation{Symbolic}}
end
export DecaSymbolic

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
    @match t begin
        Var(name) => SymbolicUtils.Sym{Number(context[name])}(name)
        Lit(v) => Meta.parse(string(v)) # TODO no YOLO
        # see heat_eq test: eqs had AppCirc1, but this returns
        # App1(f, App1(...)
        AppCirc1(fs, arg) => foldr(
            # panics with constants like :k
            # see test/language.jl
            (f, x) -> ThDEC.OPERATOR_LOOKUP[f](x),
            fs;
            init=BasicSymbolic(context, arg)
        )
        App1(f, x) => ThDEC.OPERATOR_LOOKUP[f](BasicSymbolic(context, x))
        App2(f, x, y) => ThDEC.OPERATOR_LOOKUP[f](BasicSymbolic(context, x), BasicSymbolic(context, y))
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

end
