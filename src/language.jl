@intertypes "decapodes.it" module decapodes end
using .decapodes

using Base.Iterators: partition, flatmap

term(s::Symbol) = Var(normalize_unicode(s))
term(s::Number) = Lit(Symbol(s))

term(expr::Expr) = begin
    @match expr begin
        #TODO: Would we want ∂ₜ to be used with general expressions or just Vars?
        Expr(:call, :∂ₜ, b) => Tan(Var(b))
        Expr(:call, :dt, b) => Tan(Var(b)) 

        Expr(:call, Expr(:call, :∘, a...), b) => AppCirc1(a, term(b))
        Expr(:call, a, b) => App1(a, term(b))

        Expr(:call, :+, xs...) => Plus(term.(xs))
        Expr(:call, f, x, y) => App2(f, term(x), term(y))

        # TODO: Will later be converted to Op2's or schema has to be changed to include multiplication
        Expr(:call, :*, xs...) => Mult(term.(xs))

        x => error("Cannot construct term from  $x")
    end
end

∂ = Dict(:∂ₜ¹ => 1, :∂ₜ² => 2, :∂ₜ³ => 3, :∂ₜ⁴ => 4, :∂ₜ⁵ => 5)

# TODO error handling, rhs could be symbol or expr
# we need to know the correct form 
function intercalate_dt(partial::Symbol, lhs::Symbol, rhs::Union{Symbol, Expr})
  newsyms = [gensym(lhs) for x in 1:(∂[partial]-1)];
  syms = [lhs, repeat(newsyms, inner=2)..., rhs];
  map(partition(syms, 2)) do (l, r)
    Eq(term(Expr(:call, :∂ₜ, l)), term(r))
  end
  # Judgement.(newsyms, :Form0, :I), eqns
end

# testing question:
#   quote
#     (u,v)::Form0
#     c::Constant
#
#     ∂ₜ²(u) == v
#   end
#   yields two ∂ₜ ops (as expected) which go from
#   1.u -> 5.#225, 5.#225 -> 6.#225
#   where var 6 corresponds to a sum. but why is its tag
#   the same as the gensym'd var?

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
            # IDEA:
            #   1. term(lhs) detects ∂ₜⁿu and gensyms a new variable
            #   2. new line: dₜ(n-1)(tmptan_u_n) == tmptan_u_n-1. repeat until n=1
            #   3. when n=1: 
            #
            # example: ∂ₜ³u == R
            #          ----------
            #       => ∂ₜ u == t0
            #          ∂ₜt0 == t1
            #          ∂ₜt1 == R
            #
            # intercalate_dt will produce:
            #   Eq(∂ₜ³L, R) => [Eq(term(∂ₜL), term(t0)), Eq(∂ₜt0, t1), Eq(∂ₜt2, R)]
            # which is then flatmapped out.
            # 
            # XXX will this make graphviz needlessly busy?
            # TODO can I match on a list in MLStyle, like a pattern where x can match on any 
            # of the items in the list?
            # need to parse on the rightside as well
            Expr(:call, :(==), Expr(:call, :∂ₜ², lhs), rhs) => intercalate_dt(:∂ₜ², lhs, rhs)
            Expr(:call, :(==), Expr(:call, :∂ₜ³, lhs), rhs) => intercalate_dt(:∂ₜ³, lhs, rhs)
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
# to_Decapode helper functions
### TODO - Matt: we need to generalize this
reduce_term!(t::Term, d::AbstractDecapode, syms::Dict{Symbol, Int}) =
  let ! = reduce_term!
    @match t begin
      Var(x) => begin 
        if haskey(syms, x)
           syms[x]
        else
          res_var = add_part!(d, :Var, name = x, type=:infer)
          syms[x] = res_var
        end
      end
      Lit(x) => begin 
        if haskey(syms, x)
           syms[x]
        else
          res_var = add_part!(d, :Var, name = x, type=:Literal)
          syms[x] = res_var
        end
      end
      App1(f, t) || AppCirc1(f, t) => begin
        res_var = add_part!(d, :Var, type=:infer)
        add_part!(d, :Op1, src=!(t,d,syms), tgt=res_var, op1=f)
        return res_var
      end
      App2(f, t1, t2) => begin
        res_var = add_part!(d, :Var, type=:infer)
        add_part!(d, :Op2, proj1=!(t1,d,syms), proj2=!(t2,d,syms), res=res_var, op2=f)
        return res_var
      end
      Plus(ts) => begin
        summands = [!(t,d,syms) for t in ts]
        res_var = add_part!(d, :Var, type=:infer, name=:sum)
        n = add_part!(d, :Σ, sum=res_var)
        map(summands) do s
          add_part!(d, :Summand, summand=s, summation=n)
        end
        return res_var
      end
      # TODO: Just for now assuming we have 2 or more terms
      Mult(ts) => begin
        multiplicands  = [!(t,d,syms) for t in ts]
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
      end
      Tan(t) => begin 
        # TODO: this is creating a spurious variable with the same name
        txv = add_part!(d, :Var, type=:infer)
        tx = add_part!(d, :TVar, incl=txv)
        # TODO - Matt: DerivOp being used here
        tanop = add_part!(d, :Op1, src=!(t,d,syms), tgt=txv, op1=DerivOp)
        return txv #syms[x[1]]
      end
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
