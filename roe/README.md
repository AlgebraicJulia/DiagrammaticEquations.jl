# Roe

This is a refactor of the core of diagrammatic equations, attempting to achieve a "more Julionic" approach to the problem of typed computer algebra via direct use of multiple dispatch.

## Signature

The "signature" of the DEC is encoded in a module `ThDEC`, in the following way.

First, we make a type `Sort`, elements of which represent types in the discrete exterior calculus, for instance scalars, dual/primal forms of any degree, and vector fields.

Then, we make a Julia function for each sort in the DEC. We define these Julia functions to *act on sorts*. So for instance, for the wedge product, we write a function definition like

```julia
function ∧(s1::Sort, s2::Sort)
  @match (s1, s2) begin
    (Form(i, isdual), Scalar()) || (Scalar(), Form(i, isdual)) => Form(i, isdual)
    (Form(i1, isdual), Form(i2, isdual)) =>
      if i1 + i2 <= 2
        Form(i1 + i2, isdual)
      else
        throw(SortError("Can only take a wedge product when the dimensions of the forms add to less than 2: tried to wedge product $i1 and $i2"))
      end
    _ => throw(SortError("Can only take a wedge product of two forms of the same duality"))
  end
end
```

The advantage of encoding the signature in this way is twofold.

1. We can give high-quality, context-specific errors when types fail to match.
2. It doesn't depend on any external libraries (except for MLStyle for convenience); it is just Plain Old Julia.

## Using the signature to wrap symbolic algebra

We can then wrap various symbolic frameworks by including the sorts as type parameters/metadata. For instance, for Metatheory, we create a wrapper struct `Var{s::Sort}` which wraps a `Metatheory.Id` (note that `s::Sort` rather than `s<:Sort`) We then define our methods on this struct as

```julia
unop_dec = [:∂ₜ, :d, :★, :-, :♯, :♭]
for unop in unop_dec
  @eval begin
    @nospecialize
    function $unop(v::Var{s}) where s
      s′ = $unop(s)
      Var{s′}(roe(v), addcall!(graph(v), $unop, (id(v),)))
    end

    export $unop
  end
end
```

SymbolicUtils.jl is totally analogous:

```julia
unop_dec = [:∂ₜ, :d, :★, :♯, :♭, :-]
for unop in unop_dec
  @eval begin
    @nospecialize
    function $unop(v::SymbolicUtils.BasicSymbolic{DECVar{s}}) where s
      s′ = $unop(s)
      SymbolicUtils.Term{DECVar{s′}}($unop, [v])
    end

    export $unop
  end
end
```

SymbolicUtils gets confused when the type parameter to `BasicSymbolic` is not a type: we work around this by passing in `DECVar{s}` (name subject to change), which is a zero-field struct that wraps a `Sort` as a type parameter.

## Models and namespacing

Models are then plain old Julia functions that accept as their first argument a "roe." The point of the "Roe" is to record variables that are created and equations that are asserted by the function. It looks something like:

```julia
struct Roe{T}
  vars::Dtry{T}
  eqs::Vector{Tuple{T, T}}
end
```

The type parameter `T` could be instantiated with `BasicSymbolic{<:DECVar}` or `Var`, depending on whether we are working with SymbolicUtils or Metatheory.

So, for instance, the Klausmeier model might look like

```julia
function klausmeier(roe::Roe)
  @vars roe n::DualForm0 w::DualForm0 dX::Form1 a::Constant{DualForm0} ν::Constant{DualForm0}
  @vars roe m::Number
  # The equations for our model
  @eq roe (∂ₜ(w) == a + w + (w * (n^2)) + ν * L(dX,w))
  @eq roe (∂ₜ(n) == w * n^2 - m*n + Δ(n))
end
```

Namespacing is achieved by moving the roe into a namespace before passing it into submodels. So, for instance, to make a model with two Klausmeier submodels that share the same `m`, we could do:

```julia
function double_klausmeier(roe::Roe)
  klausmeier(namespaced(roe, :k1))
  klausmeier(namespaced(roe, :k2))

  @eq roe (roe.k1.m == roe.k2.m)
end
```

The implementation of `namespace` would be something like

```julia
function namespace(roe::Roe{T}, name::Symbol) where {T}
  Roe(get(roe.vars, name, Dtry{T}()), roe.eqs)
end
```

Here, `get` either gets a pre-existing subnamespace at `name`, or creates a new subnamespace and inserts it at `name`. An alternative implementation would have a `namespace::Vec{Symbol}` field on `Roe` which is used to prefix every newly created variable.

An alternative to adding the equation `roe.k1.m == roe.k2.m` would be to have `klausmeier` take `m` as a parameter, which would look like

```julia
function klausmeier(roe::Roe, m)
  @vars roe n::DualForm0 w::DualForm0 dX::Form1 a::Constant{DualForm0} ν::Constant{DualForm0}
  # The equations for our model
  @eq roe (∂ₜ(w) == a + w + (w * (n^2)) + ν * L(dX,w))
  @eq roe (∂ₜ(n) == w * n^2 - m*n + Δ(n))
end

function double_klausemeier(roe::Roe)
  @vars roe m::Number

  klausmeier(namespaced(roe, :k1), m)
  klausmeier(namespaced(roe, :k2), m)
end
```
