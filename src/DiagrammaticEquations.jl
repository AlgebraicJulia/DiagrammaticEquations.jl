""" The DiagrammaticEquations module exports data structures which represent diagrammatic equations and functions which manipulate them.
"""
module DiagrammaticEquations

export
DerivOp, append_dot, normalize_unicode, 

## intertypes
SchDecapode, SchNamedDecapode, AbstractDecapode, AbstractNamedDecapode, NamedDecapode, DecaExpr, Plus, AppCirc1, Var, Tan, App1, App2, Eq

using Catlab
using Catlab.Theories
import Catlab.Theories: otimes, oplus, compose, ⊗, ⊕, ⋅, associate, associate_unit, Ob, Hom, dom, codom
using Catlab.Programs
using Catlab.CategoricalAlgebra
import Catlab.CategoricalAlgebra: ∧
using Catlab.WiringDiagrams
using Catlab.WiringDiagrams.DirectedWiringDiagrams
using Catlab.ACSetInterface
using MLStyle
import Unicode
using Reexport

## TODO:
## generate schema from a _theory_
## from presentation in the theory determine what to slice over
## Pavel S., Hypergraph theory string diagram
normalize_unicode(s::String) = Unicode.normalize(s, compose=true, stable=true, chartransform=Unicode.julia_chartransform)
normalize_unicode(s::Symbol)  = Symbol(normalize_unicode(String(s)))
DerivOp = Symbol("∂ₜ")
append_dot(s::Symbol) = Symbol(string(s)*'\U0307')

include("acset.jl")
include("language.jl")
include("composition.jl")
include("collages.jl")
include("visualization.jl")
include("rewrite.jl")
include("pretty.jl")
include("colanguage.jl")
include("openoperators.jl")
include("deca/Deca.jl")
include("learn/Learn.jl")
include("ThDEC.jl")
include("decasymbolic.jl")

@reexport using .ThDEC
@reexport using .SymbolicUtilsInterop
@reexport using .Deca


end
