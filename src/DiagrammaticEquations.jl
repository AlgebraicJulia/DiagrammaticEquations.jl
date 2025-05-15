""" The DiagrammaticEquations module exports data structures which represent diagrammatic equations and functions which manipulate them.
"""
module DiagrammaticEquations

using Catlab

export
DerivOp, append_dot, normalize_unicode, infer_states, infer_types!,
# Deca
op1_operators, op1_1D_bound_operators, op1_2D_bound_operators, op2_operators,
recursive_delete_parents, spacename, varname, unicode!, vec_to_dec!,
## collages
Collage, collate,
## composition
oapply, unique_by, unique_by!, OpenSummationDecapodeOb, OpenSummationDecapode, Open, default_composition_diagram,
apex, @relation, # Re-exported from Catlab
## acset
SchDecapode, SchNamedDecapode, AbstractDecapode, AbstractNamedDecapode, NamedDecapode, SummationDecapode,
contract_operators!, contract_operators, add_constant!, add_parameter, fill_names!, dot_rename!, is_expanded, expand_operators, infer_state_names, infer_terminal_names, recognize_types,
resolve_overloads!, replace_names!, type_check, check_rule_ambiguity,
transfer_parents!, transfer_children!,
unique_lits!,
## language
@decapode, Term, parse_decapode, term, Eq, DecaExpr,
# ~~~~~
Plus, AppCirc1, Var, Tan, App1, App2,
## Parser
@decapode_str,
## visualization
to_graphviz_property_graph, typename, draw_composition,
to_graphviz, # Re-exported from Catlab
## rewrite
average_rewrite,
## openoperators
transfer_parents!, transfer_children!, replace_op1!, replace_op2!, replace_all_op1s!, replace_all_op2s!,
Rule, infer_resolve!, type_check, DecaTypeExeception


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
include("parser.jl")
include("composition.jl")
include("collages.jl")
include("visualization.jl")
include("rewrite.jl")
include("pretty.jl")
include("colanguage.jl")
include("openoperators.jl")
include("symbolictheoryutils.jl")
include("graph_traversal.jl")
include("deca/Deca.jl")
include("learn/Learn.jl")
include("SymbolicUtilsInterop.jl")

@reexport using .Deca
@reexport using .SymbolicUtilsInterop

include("acset2symbolic.jl")

end