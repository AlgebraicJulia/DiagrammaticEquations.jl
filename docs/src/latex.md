# LaTeX Rendering

`DiagrammaticEquations.jl` can render equations from a Decapode eDSL block as LaTeX strings.

```@example latex-rendering
using DiagrammaticEquations

eqs = decapode_latex_strings(quote
  h::Form0
  Γ::Constant
  ∂ₜ(h) == Γ * h
  q == h + Γ
end)

eqs
```

For notebook-style LaTeX display, use `decapode_latex` (or `@decapode_latex`) which provides a `text/latex` show method.

```@example latex-rendering
decapode_latex(quote
  h::Form0
  Γ::Form0
  n::Constant

  ∂ₜ(h) == Γ * δ(d(h) ∧ mag(♯(d(h))) ^ (n - 1) ∧ h ^ (n + 2))
end)
```
