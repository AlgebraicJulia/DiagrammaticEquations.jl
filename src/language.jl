@intertypes "decapodes.it" module decapodes end
using .decapodes

term(s::Symbol) = Var(normalize_unicode(s))
term(s::Number) = Lit(Symbol(s))

"""Recursively collect variable names appearing in a `Term` into `variables`."""
function _collect_term_variables!(variables::Set{Symbol}, t::Term)
  @match t begin
    Var(name) => push!(variables, name)
    Lit(_) => nothing
    AppCirc1(_, arg) || App1(_, arg) || Tan(arg) => _collect_term_variables!(variables, arg)
    App2(_, arg1, arg2) => begin
      _collect_term_variables!(variables, arg1)
      _collect_term_variables!(variables, arg2)
    end
    Plus(args) || Mult(args) => foreach(arg -> _collect_term_variables!(variables, arg), args)
  end
  variables
end

"""Return sorted unique variable names that appear in a `Term` for deterministic output."""
term_variables(t::Term) = sort!(collect(_collect_term_variables!(Set{Symbol}(), t)))

"""Compute a downset from the variables referenced in a `Term` or expression."""
downset(d::SummationDecapode, t::Term) = downset(d, term_variables(t))
downset(d::SummationDecapode, expr::Expr) = downset(d, term(expr))

"""Mutating form of `downset` for `Term` or expression inputs."""
downset!(e::SummationDecapode, d::SummationDecapode, t::Term) = downset!(e, d, term_variables(t))
downset!(e::SummationDecapode, d::SummationDecapode, expr::Expr) = downset!(e, d, term(expr))

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

function parse_decapode(expr::Expr)
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
    end |> skipmissing |> Base.collect
    judges = []
    eqns = []
    foreach(stmts) do s
      @match s begin
        ::Judgement => push!(judges, s)
        ::Vector{Judgement} => append!(judges, s)
        ::Eq => push!(eqns, s)
        _ => error("Statement containing $s of type $(typeof(s)) was not added.")
      end
    end
    DecaExpr(judges, eqns)
end

# to_Decapode helper functions
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
  :(SummationDecapode(parse_decapode($(Meta.quot(e))))) |> esc
end

struct DecapodeLaTeX
  equations::Vector{String}
end

# Normalize the ASCII alias "wedge(a, b)" → "∧(a, b)" so that Latexify
# renders it infix via the binary_operators table registered in __init__.
_norm_wedge(s::Symbol) = s === :wedge ? :∧ : s
_norm_wedge(x) = x
_norm_wedge(e::Expr) = Expr(e.head, map(_norm_wedge, e.args)...)

function _latexify_statement(expr::Expr)
  latex = String(latexify(_norm_wedge(expr)))
  startswith(latex, '$') && endswith(latex, '$') ? latex[2:end-1] : latex
end

function decapode_latex_strings(e::Expr)
  lines = e.head === :block ? e.args : Any[e]
  eqs = map(lines) do line
    @match line begin
      Expr(:call, :(==), lhs, rhs) => _latexify_statement(line)
      _ => nothing
    end
  end
  filter!(!isnothing, eqs)
  eqs
end

decapode_latex(e::Expr) = DecapodeLaTeX(decapode_latex_strings(e))

"""
    macro decapode_latex(e)

Create a `DecapodeLaTeX` display object from a Decapode eDSL block.

This macro mirrors `@decapode` syntax while returning a render-only object for
notebook environments. The resulting object supports `show(::MIME"text/latex", ...)`
to display the equations in aligned LaTeX form.
"""
macro decapode_latex(e)
  :(decapode_latex($(Meta.quot(e)))) |> esc
end

Base.show(io::IO, d::DecapodeLaTeX) = print(io, join(d.equations, '\n'))
function Base.show(io::IO, ::MIME"text/latex", d::DecapodeLaTeX)
  print(io, raw"\[", raw"\begin{aligned}", join(d.equations, raw" \\ "), raw"\end{aligned}", raw"\]")
end

# Verify that @decapode is usable at module-level in source (not just in tests/REPL).
const _LANGUAGE_CHECK = @decapode begin
  A::Form0{X}
  B::Form1{X}
  B == d₀(A)
end
