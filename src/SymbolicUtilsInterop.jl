module SymbolicUtilsInterop

using ACSets
using ..DiagrammaticEquations: AbstractDecapode, Quantity, DerivOp
using ..DiagrammaticEquations: recognize_types, fill_names!, make_sum_mult_unique!
import ..DiagrammaticEquations: eval_eq!, SummationDecapode
using ..decapodes
using ..Deca

using MLStyle
using SymbolicUtils
using SymbolicUtils: Symbolic, BasicSymbolic, FnType, Sym, symtype

# name collision with decapodes.Equation
struct SymbolicEquation{E}
    lhs::E
    rhs::E
end
export SymbolicEquation

Base.show(io::IO, e::SymbolicEquation) = print(io, "$(e.lhs) == $(e.rhs)")

## a struct carry the symbolic variables and their equations
struct SymbolicContext
    vars::Vector{Symbolic}
    equations::Vector{SymbolicEquation{Symbolic}}
end
export SymbolicContext

Base.show(io::IO, d::SymbolicContext) = begin
    println(io, "SymbolicContext(")
    println(io, "  Variables: [$(join(d.vars, ", "))]")
    println(io, "  Equations: [")
    eqns = map(d.equations) do op
      "    $(op)"
    end
    println(io, "$(join(eqns,",\n"))])")
  end

## BasicSymbolic -> DecaExpr
function decapodes.Term(t::SymbolicUtils.BasicSymbolic)
    if SymbolicUtils.issym(t)
        decapodes.Var(nameof(t))
    else
        op = SymbolicUtils.head(t)
        args = SymbolicUtils.arguments(t)
        termargs = Term.(args)
        if op == +
            decapodes.Plus(termargs)
        elseif op == *
            decapodes.Mult(termargs)
        elseif op ∈ [DerivOp, ∂ₜ]
            decapodes.Tan(only(termargs))
        elseif length(args) == 1
            decapodes.App1(nameof(op, symtype.(args)...), termargs...)
        elseif length(args) == 2
            decapodes.App2(nameof(op, symtype.(args)...), termargs...)
        else
            error("was unable to convert $t into a Term")
        end
    end
end
# TODO subtraction is not parsed as such. e.g.,
# a, b = @syms a::Scalar b::Scalar
# Term(a-b) = Plus(Term[Var(:a), Mult(Term[Lit(Symbol("-1")), Var(:b)]))

decapodes.Term(x::Real) = decapodes.Lit(Symbol(x))

function decapodes.DecaExpr(d::SymbolicContext)
    context = map(d.vars) do var
        decapodes.Judgement(nameof(var), nameof(symtype(var)), :I)
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
function SymbolicUtils.BasicSymbolic(context::Dict{Symbol,DataType}, t::decapodes.Term)
    # user must import symbols into scope
    ! = (f -> getfield(@__MODULE__, f))
    @match t begin
        Var(name) => SymbolicUtils.Sym{context[name]}(name)
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
        App1(f, x) => (!(f))(BasicSymbolic(context, x))
        App2(f, x, y) => (!(f))(BasicSymbolic(context, x), BasicSymbolic(context, y))
        Plus(xs) => +(BasicSymbolic.(Ref(context), xs)...)
        Mult(xs) => *(BasicSymbolic.(Ref(context), xs)...)
        Tan(x) => (!(DerivOp))(BasicSymbolic(context, x))
    end
end

function SymbolicContext(d::decapodes.DecaExpr)
    # associates each var to its sort...
    context = map(d.context) do j
        j.var => symtype(Deca.DECQuantity, j.dim, j.space)
    end
    # ... which we then produce a vector of symbolic vars
    vars = map(context) do (v, s)
        SymbolicUtils.Sym{s}(v)
    end
    context = Dict{Symbol,DataType}(context)
    eqs = map(d.equations) do eq
        SymbolicEquation{Symbolic}(BasicSymbolic.(Ref(context), [eq.lhs, eq.rhs])...)
    end
    SymbolicContext(vars, eqs)
end

function eval_eq!(eq::SymbolicEquation, d::AbstractDecapode, syms::Dict{Symbol, Int}, deletions::Vector{Int})
    eval_eq!(Eq(Term(eq.lhs), Term(eq.rhs)), d, syms, deletions)
end

"""    function SummationDecapode(e::SymbolicContext) """
function SummationDecapode(e::SymbolicContext)
    d = SummationDecapode{Any, Any, Symbol}()
    symbol_table = Dict{Symbol, Int}()

    foreach(e.vars) do var
        # convert Sort(var)::PrimalForm0 --> :Form0
        var_id = add_part!(d, :Var, name=var.name, type=nameof(symtype(var)))
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

end
