# DiagrammaticEquations.jl

[![Stable Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](https://AlgebraicJulia.github.io/DiagrammaticEquations.jl/stable)
[![Development Documentation](https://img.shields.io/badge/docs-dev-blue.svg)](https://AlgebraicJulia.github.io/DiagrammaticEquations.jl/dev)
[![Code Coverage](https://codecov.io/gh/AlgebraicJulia/DiagrammaticEquations.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/AlgebraicJulia/DiagrammaticEquations.jl)
[![CI/CD](https://github.com/AlgebraicJulia/DiagrammaticEquations.jl/actions/workflows/julia_ci.yml/badge.svg)](https://github.com/AlgebraicJulia/DiagrammaticEquations.jl/actions/workflows/julia_ci.yml)

`DiagrammaticEquations.jl` is a Julia library which uses category theory to represent and manipulate systems of equations. An embedded DSL for representing [`Decapodes.jl`](https://github.com/AlgebraicJulia/Decapodes.jl) multiphysics equations is offered, as well as high-level tools for composing them. This DSL and associated tools feature:

- Type checking
- Type inference
- Function resolution
- Common subexpression elimination
- Safe, namespaced composition
- Pretty printing
- Serialization to JSON
- Rewriting
- High-level boundary condition specification
- Interop for symbolic manipulation
- Compute graph visualization
- Categorical algebra features (homomorphism checking, etc.)

Many of these capabilities are demonstrated in the documentation of our [`Decapodes.jl`](https://github.com/AlgebraicJulia/Decapodes.jl) package.

## Relevant Work
* Patterson, E., Baas, A., Hosgood, T., & Fairbanks, J. (2022). A diagrammatic view of differential equations in physics. arXiv preprint arXiv:2204.01843.
