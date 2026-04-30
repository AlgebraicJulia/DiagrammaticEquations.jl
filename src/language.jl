@intertypes "decapodes.it" module decapodes end
using .decapodes

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

_strip_dollars(s::AbstractString) =
  startswith(s, '$') && endswith(s, '$') ? s[2:end-1] : s

# Returns true when `op` is a binary operator that should be rendered infix,
# i.e. wedge product and its subscripted variants (∧, ∧₀₁, ∧₁₀, …) plus the
# ASCII alias "wedge".
_is_infix_binary_op(op) = op isa Symbol &&
  (String(op) == "wedge" || startswith(String(op), "∧"))

# LaTeX string for the infix operator symbol.
function _infix_op_latex(op::Symbol)
  op === :wedge && return raw"\wedge"
  _strip_dollars(String(latexify(op)))  # handles ∧ → \wedge, ∧₀₁ → \wedge_{0 1}, etc.
end

# True when the expression tree rooted at `expr` contains at least one
# infix binary operator call.
function _has_infix_binary_op(expr)
  expr isa Expr || return false
  if expr.head === :call
    op = expr.args[1]
    _is_infix_binary_op(op) && length(expr.args) == 3 && return true
  end
  any(_has_infix_binary_op, expr.args)
end

# Recursively convert a Julia expression to a LaTeX string, using infix
# notation for binary operators such as the wedge product.  For expressions
# that contain no infix binary operators the result is identical to calling
# `latexify` directly.
function _expr_to_latex(expr)
  if expr isa Expr && expr.head === :call
    op = expr.args[1]
    args = expr.args[2:end]
    if _is_infix_binary_op(op) && length(args) == 2
      lhs = _expr_to_latex(args[1])
      rhs = _expr_to_latex(args[2])
      return "$lhs $(_infix_op_latex(op)) $rhs"
    end
    _has_infix_binary_op(expr) && return _build_infix_latex(expr)
  elseif expr isa Expr && _has_infix_binary_op(expr)
    return _build_infix_latex(expr)
  end
  _strip_dollars(String(latexify(expr)))
end

# Handle expression types that may contain infix binary ops nested inside them.
function _build_infix_latex(expr::Expr)
  expr.head !== :call && return _strip_dollars(String(latexify(expr)))
  op   = expr.args[1]
  args = expr.args[2:end]
  if op === :(==) && length(args) == 2
    return "$(_expr_to_latex(args[1])) = $(_expr_to_latex(args[2]))"
  elseif op === :+
    return join(map(_expr_to_latex, args), " + ")
  elseif op === :- && length(args) == 2
    return "$(_expr_to_latex(args[1])) - $(_expr_to_latex(args[2]))"
  elseif op === :- && length(args) == 1
    return "-$(_expr_to_latex(args[1]))"
  elseif op === :*
    return join(map(_expr_to_latex, args), " \\cdot ")
  elseif op === :^ && length(args) == 2
    return "{$(_expr_to_latex(args[1]))}^{$(_expr_to_latex(args[2]))}"
  else
    # Generic function application: f(a, b, …)
    fn_latex   = _strip_dollars(String(latexify(op)))
    args_latex = map(_expr_to_latex, args)
    return "$fn_latex\\left($(join(args_latex, ", "))\\right)"
  end
end

function _latexify_statement(expr::Expr)
  _expr_to_latex(expr)
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
