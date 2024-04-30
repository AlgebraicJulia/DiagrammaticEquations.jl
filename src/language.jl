@intertypes "decapodes.it" module decapodes end
using .decapodes

using Base.Iterators: partition

term(s::Symbol) = Var(normalize_unicode(s))
term(s::Number) = Lit(Symbol(s))

term(expr::Expr) = begin
    @match expr begin
        #TODO: Would we want ∂ₜ to be used with general expressions or just Vars?
        Expr(:call, :∂ₜ, b) => Tan(Var(b))
        Expr(:call, :dt, b) => Tan(Var(b))
        Expr(:call, ∂, b) && if ishigherorderpartial(∂) end => Partial(term(b), ∂s[∂]..., 0) 

        Expr(:call, Expr(:call, :∘, a...), b) => AppCirc1(a, term(b))
        Expr(:call, a, b) => App1(a, term(b))

        Expr(:call, :+, xs...) => Plus(term.(xs))
        Expr(:call, f, x, y) => App2(f, term(x), term(y))

        # TODO: Will later be converted to Op2's or schema has to be changed to include multiplication
        Expr(:call, :*, xs...) => Mult(term.(xs))

        x => error("Cannot construct term from  $x")
    end
end

ishigherorderpartial(t::Symbol) = haskey(∂s, t)
ishigherorderpartial(t::Expr) = @match t begin
  Expr(:call, ∂, _) => ishigherorderpartial(∂)
  _ => false
end

∂s = Dict(:∂ₜ¹ => (:t, 1), :∂ₜ² => (:t, 2), :∂ₜ³ => (:t, 3), :∂ₜ⁴ => (:t, 4), :∂ₜ⁵ => (:t, 5))
ps = Dict(1 => :∂ₜ, 2 => :∂ₜ², 3 => :∂ₜ³)

function parse_decapode(expr::Expr)
    # XXX how does flatmap affect maps in this match expression?
    stmts = map(expr.args) do line 
        @match line begin
            ::LineNumberNode => missing
            # TODO: If user doesn't provide space, this gives a temp space so we can continue to construction
            # For now spaces don't matter so this is fine but if they do, this will need to change
            Expr(:(::), a::Symbol, b::Symbol) => Judgement(a, b, :I)
            Expr(:(::), a::Expr, b::Symbol) => map(sym -> Judgement(sym, b, :I), a.args)

            Expr(:(::), a::Symbol, b) => Judgement(a, b.args[1], b.args[2])
            Expr(:(::), a::Expr, b) => map(sym -> Judgement(sym, b.args[1], b.args[2]), a.args)
            Expr(:call, :(==), lhs, rhs) => Eq(term(lhs), term(rhs))
            _ => error("The line $line is malformed")
        end
    end |> skipmissing |> collect
    judges = []
    eqns = []
    foreach(stmts) do s
      @match s begin
        ::Judgement => push!(judges, s)
        ::Vector{Judgement} => append!(judges, s)
        ::Eq => push!(eqns, s)
        ::Vector{Eq} => append!(eqns, s)
        ::Tuple{Vector{Judgement}, Vector{Eq}} => (append!(judges, s[1]), append!(eqns, s[2]))
        _ => error("Statement containing $s of type $(typeof(s)) was not added.")
      end
    end
    DecaExpr(judges, eqns)
end

function reduce_term_var!(x::Symbol, d::AbstractDecapode, syms::Dict{Symbol, Int})
  haskey(syms, x) ? syms[x] : syms[x] = add_part!(d, :Var, name = x, type = :infer)
end

function reduce_term_lit!(x::Symbol, d::AbstractDecapode, syms::Dict{Symbol, Int})
  haskey(syms, x) ? syms[x] : syms[x] = add_part!(d, :Var, name = x, type = :Literal)
end

function reduce_term_app1circ!(f, t::Term, d::AbstractDecapode, syms::Dict{Symbol, Int})
  res_var = add_part!(d, :Var, type = :infer)
  add_part!(d, :Op1, src=reduce_term!(t, d, syms), tgt=res_var, op1=f)
  return res_var
end

function reduce_term_app2!(f, arg1::Term, arg2::Term, d::AbstractDecapode, syms::Dict{Symbol, Int})
  res_var = add_part!(d, :Var, type=:infer)
  add_part!(d, :Op2, proj1=reduce_term!(arg1,d,syms), proj2=reduce_term!(arg2,d,syms), res=res_var, op2=f)
  return res_var
end

function reduce_term_plus!(ts::Vector{Term}, d::AbstractDecapode, syms::Dict{Symbol, Int})
  summands = reduce_term!.(ts, Ref(d), Ref(syms))
  res_var = add_part!(d, :Var, type=:infer, name=:sum)
  n = add_part!(d, :Σ, sum=res_var)
  foreach(summands) do s
    add_part!(d, :Summand, summand=s, summation=n)
  end
  return res_var
end

const MULT_SYM = Symbol("*")

# TODO: res=res_var undefined
function reduce_term_mult!(ts::Vector{Term}, d::AbstractDecapode, syms::Dict{Symbol, Int})
        multiplicands  = [reduce_term!(t,d,syms) for t in ts]
        res_var = add_part!(d, :Var, type=:infer, name=:mult)
        m1,m2 = multiplicands[1:2]
        add_part!(d, :Op2, proj1=m1, proj2=m2, res=res_var, op2=Symbol("*"))
        for m in multiplicands[3:end]
          m1 = res_var
          m2 = m
          res_var = add_part!(d, :Var, type=:infer, name=:mult)
          add_part!(d, :Op2, proj1=m1, proj2=m2, res=res_var, op2=Symbol("*"))
        end
    return res_var
#   multiplicands = [reduce_term!.(ts, Ref(d), Ref(syms))]
#   out = foldl(((m1, m2) -> add_part!(d, :Op2, proj1=m1, proj2=m2, res=m1, op2=MULT_SYM))
#         ,multiplicands, add_part!(d, :Var, type=:infer, name=:mult))
#   @info "MULT" out
#   out
end

append_doot(s) = Symbol(string(s)*"_1")

# TODO do we want to 1) continue using DerivOp and 2) create a spurious var. with same name?
function reduce_term_tan!(t::Term, d::AbstractDecapode, syms::Dict{Symbol, Int})
  txv = add_part!(d, :Var, type=:infer, name=append_doot(t.name)) 
  tx = add_part!(d, :TVar, incl=txv) # cache tvars in a list
  src = incident(d, t.name, :name)[1]
  tanop = add_part!(d, :Op1, src=src, tgt=txv, op1=DerivOp)
  return txv
end

function _reduce_term_partial!(t::Term, d::AbstractDecapode, syms::Dict{Symbol, Int})
  txv = reduce_term_tan!(t.var, d, syms)
  reduce_term!(Partial(Var(d[txv,:name]), t.wrt, t.order-1, t.iteration+1), d, syms)
  return txv
end

# TODO george said something about this
function reduce_term_partial!(t::Term, d::AbstractDecapode, syms::Dict{Symbol, Int})
  txv = reduce_term!(Partial(t.var, t.wrt, t.order - 1, 0), d, syms)
  txv = reduce_term_tan!(Var(d[txv,:name]), d, syms)
  return txv
end

function reduce_term!(t::Term, d::AbstractDecapode, syms::Dict{Symbol, Int})
  @match t begin
    Var(x) => reduce_term_var!(x, d, syms)
    Lit(x) => reduce_term_lit!(x, d, syms)
    App1(f, t) || AppCirc1(f, t) => reduce_term_app1circ!(f, t, d, syms)
    App2(f, t1, t2) => reduce_term_app2!(f, t1, t2, d, syms)
    Plus(ts) => reduce_term_plus!(ts, d, syms)
    Mult(ts) => reduce_term_mult!(ts, d, syms)
    Tan(t) || Partial(t, wrt, 1, k) => reduce_term_tan!(t, d, syms)
    Partial(u, wrt, n, k) => reduce_term_partial!(t, d, syms)
    _ => throw("Inline type Judgements not yet supported!")
  end
end

function eval_eq!(eq::Equation, d::AbstractDecapode, syms::Dict{Symbol, Int}, deletions::Vector{Int}) 
  @match eq begin
    Eq(t1, t2) => begin
      lhs_ref = reduce_term!(t1,d,syms)
      rhs_ref = reduce_term!(t2,d,syms)

      # Always let the a named variable take precedence 
      # TODO: If we have variable to variable equality, we want
      # some kind of way to check track of this equality
      ref_pair = (t1, t2)
      @match ref_pair begin
        (Var(a), Var(b)) => return d
        (t1, Var(b)) => begin
          lhs_ref, rhs_ref = rhs_ref, lhs_ref
        end
        _ => nothing
      end

      # Make rhs_ref equal to lhs_ref and adjust all its incidents

      # Case rhs_ref is a Tan
      # WARNING: Don't push to deletion here because all TanVars should have a 
      # corresponding Op1. Pushing here would create a duplicate which breaks rem_parts!
      for rhs in incident(d, rhs_ref, :incl)
        d[rhs, :incl] = lhs_ref
      end
      # Case rhs_ref is a Op1
      for rhs in incident(d, rhs_ref, :tgt)
        d[rhs, :tgt] = lhs_ref
        push!(deletions, rhs_ref)
      end
      # Case rhs_ref is a Op2
      for rhs in incident(d, rhs_ref, :res)
        d[rhs, :res] = lhs_ref
        push!(deletions, rhs_ref)
      end
      # Case rhs_ref is a Plus
      # FIXME: this typeguard is a subsitute for refactoring into multiple dispatch
      if isa(d, SummationDecapode)
        for rhs in incident(d, rhs_ref, :sum)
          d[rhs, :sum] = lhs_ref
          push!(deletions, rhs_ref)
        end
      end
      # TODO: delete unused vars. The only thing stopping me from doing 
      # this is I don't know if CSet deletion preserves incident relations
      #rem_parts!(d, :Var, sort(deletions))
    end
  end
  return d
end

"""    function Decapode(e::DecaExpr)

Takes a DecaExpr and returns a Decapode ACSet.
"""
function Decapode(e::DecaExpr)
  d = Decapode{Any, Any}()
  symbol_table = Dict{Symbol, Int}()
  for Judgement in e.context
    var_id = add_part!(d, :Var, type=(Judgement.dim, Judgement.space))
    symbol_table[Judgement.var.name] = var_id
  end
  deletions = Vector{Int}()
  for eq in e.equations
    eval_eq!(eq, d, symbol_table, deletions)
  end
  rem_parts!(d, :Var, sort(deletions))
  recognize_types(d)
  return d
end

"""    function SummationDecapode(e::DecaExpr)

Takes a DecaExpr and returns a SummationDecapode ACSet.
"""
function SummationDecapode(e::DecaExpr)
    d = SummationDecapode{Any, Any, Symbol}()
    symbol_table = Dict{Symbol, Int}()

    for judgement in e.context
      var_id = add_part!(d, :Var, name=judgement.var, type=judgement.dim)
      symbol_table[judgement.var] = var_id
    end

    deletions = Vector{Int}()
    for eq in e.equations
      eval_eq!(eq, d, symbol_table, deletions)
    end
    rem_parts!(d, :Var, sort(deletions))

    recognize_types(d)

    fill_names!(d)
    d[:name] = normalize_unicode.(d[:name])
    make_sum_mult_unique!(d)
    return d
end


"""    macro decapode(e)

Construct a SummationDecapode using the Decapode Domain-Specific Language.
"""
macro decapode(e)
  :(SummationDecapode(parse_decapode($(Meta.quot(e)))))
end
