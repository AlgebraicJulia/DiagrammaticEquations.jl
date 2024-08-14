module SymbolicUtilInterop

using ..ThDEC
using MLStyle
import ..ThDEC: Sort, dim, isdual
using ..decapodes
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic

abstract type DECType <: Number end

"""
i: dimension: 0,1,2, etc.
d: duality: true = dual, false = primal
"""
struct FormT{i,d} <: DECType
end

struct VFieldT{d} <: DECType
end

dim(::Type{<:FormT{d}}) where {d} = d
isdual(::Type{FormT{i,d}}) where {i,d} = d

# convenience functions
const PrimalFormT{i} = FormT{i,false}
export PrimalFormT

const DualFormT{i} = FormT{i,true}
export DualFormT

const PrimalVFT = VFieldT{false}
export PrimalVFT

const DualVFT = VFieldT{true}
export DualVFT

function Sort(::Type{FormT{i,d}}) where {i,d}
    Form(i, d)
end

function Number(f::Form)
    FormT{dim(f),isdual(f)}
end

function Sort(::Type{VFieldT{d}}) where {d}
    VField(d)
end

function Number(v::VField)
    VFieldT{isdual(v)}
end

function Sort(::Type{<:Real})
    Scalar()
end

function Number(s::Scalar)
    Real
end

function Sort(::BasicSymbolic{T}) where {T}
    Sort(T)
end

function Sort(::Real)
    Scalar()
end

unop_dec = [:∂ₜ, :d, :★, :♯, :♭, :-]
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

struct Equation{E}
    lhs::E
    rhs::E
end

struct DecaSymbolic
    vars::Vector{Symbolic}
    equations::Vector{Equation{Symbolic}}
end

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

function decapodes.Term(x::Real)
    decapodes.Lit(Symbol(x))
end

function decapodes.DecaExpr(d::DecaSymbolic)
    context = map(d.vars) do var
        decapodes.Judgement(nameof(var), nameof(Sort(var)), :I)
    end
    equations = map(d.equations) do eq
        decapodes.Eq(decapodes.Term(eq.lhs), decapodes.Term(eq.rhs))
    end
    decapodes.DecaExpr(context, equations)
end

function SymbolicUtils.BasicSymbolic(context::Dict{Symbol,Sort}, t::decapodes.Term)
    @match t begin
        Var(name) => SymbolicUtils.Sym{Number(context[name])}(name)
        Lit(v) => Meta.parse(string(v)) # YOLO
        AppCirc1(fs, arg) => foldr(
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

function DecaSymbolic(d::decapodes.DecaExpr)
    context = map(d.context) do j
        j.var => ThDEC.SORT_LOOKUP[j.dim]
    end
    vars = map(context) do (v, s)
        SymbolicUtils.Sym{Number(s)}(v)
    end
    context = Dict{Symbol,Sort}(context)
    eqs = map(d.equations) do eq
        Equation{Symbolic}(BasicSymbolic(context, eq.lhs), BasicSymbolic(context, eq.rhs))
    end
    DecaSymbolic(vars, eqs)
end

end
