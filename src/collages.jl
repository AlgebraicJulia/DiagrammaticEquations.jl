struct Collage
  src::SummationDecapode{Any,Any,Symbol}
  tgt::SummationDecapode{Any,Any,Symbol}
  uwd::Catlab.Programs.RelationalPrograms.UntypedUnnamedRelationDiagram{Symbol, Symbol}
  symbols::Dict{Symbol, Symbol}
end

collate(c::Collage) = collate(c.src, c.tgt, c.uwd, c.symbols)

# TODO: This is assuming only "restriction"-type morphisms.
# TODO: Throw an error if the user tries to use a boundary value differential
# form that is of a different type of the thing that we are applying the bound
# to. i.e. Form1 but target is a Form0.
"""    function collate(equations, boundaries, uwd, symbols)

Create a collage of two Decapodes that simulates with boundary conditions.
```
"""
function collate(equations, boundaries, uwd, symbols)
  f = SummationDecapode{Any, Any, Symbol}()
  copy_parts!(f, equations, (:Var, :TVar, :Op1, :Op2, :Σ, :Summand))
  for b in boxes(uwd)
    # Set up pointers.
    ps = incident(uwd, b, :box)
    ev = first(ps)
    bv = last(ps)
    en_key = uwd[junction(uwd, ev), :variable]
    bn_key = uwd[junction(uwd, bv), :variable]
    en = symbols[en_key]
    bn = symbols[bn_key]
    var = only(incident(f, en, :name))
    # Add a new variable and transfer the children of the original variable to it.
    b_var = add_part!(f, :Var, type=f[var, :type], name=Symbol("r$(b)_" * string(f[var, :name])))
    transfer_children!(f, var, b_var)
    # Transfer ∂ₜ morphisms back to the original variable, if any.
    transferred_partials = filter(x -> f[x, :op1]==:∂ₜ, incident(f, b_var, :src))
    if !isempty(transferred_partials)
      f[only(transferred_partials), :src] = var
    end
    # Transfer ∂ₜ morphisms to the "masked"/ bounded variable, if any.
    untransferred_partials = filter(x -> f[x, :op1]==:∂ₜ, incident(f, var, :tgt))
    if !isempty(untransferred_partials)
      f[untransferred_partials, :tgt] = b_var
    end
    # Insert the "masking" operation.
    gettype(xs, n) = xs[only(incident(xs, n, :name)), :type]
    newtype = @match (gettype(equations, en), gettype(boundaries, bn)) begin
        (:infer, _) => :infer
        (_, :infer) => :infer
        (:Form0, :Constant) => :Form0
        (x, y) && if x == y end => x
        (x, y) => error("Type mismatch between $x and $y")
    end
    s_var = add_part!(f, :Var, type=newtype, name=bn)
    add_part!(f, :Op2, proj1=var, proj2=s_var, res=b_var, op2=uwd[b, :name])
  end

  f
end
