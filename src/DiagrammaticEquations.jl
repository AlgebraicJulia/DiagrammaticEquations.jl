""" The DiagrammaticEquations module exports data structures which represent diagrammatic equations and functions which manipulate them.
"""
module DiagrammaticEquations

export
DerivOp, append_dot, normalize_unicode, infer_states, infer_types!,
# Deca
op1_res_rules_1D, op2_res_rules_1D, op1_res_rules_2D, op2_res_rules_2D,
op1_inf_rules_1D, op2_inf_rules_1D, op1_inf_rules_2D, op2_inf_rules_2D,
recursive_delete_parents, spacename, varname, unicode!, vec_to_dec!,
## collages
Collage, collate,
## composition
oapply, unique_by, unique_by!, OpenSummationDecapodeOb, OpenSummationDecapode, Open,
## acset
SchDecapode, SchNamedDecapode, AbstractDecapode, AbstractNamedDecapode, NamedDecapode, SummationDecapode,
contract_operators!, contract_operators, add_constant!, add_parameter, fill_names!, dot_rename!, expand_operators, infer_state_names, recognize_types,
resolve_overloads!, replace_names!,
apply_inference_rule_op1!, apply_inference_rule_op2!,
## language
@decapode, Term, parse_decapode, term, Eq, DecaExpr,
# ~~~~~
Plus, AppCirc1, Var, Tan, App1, App2,
## visualization
to_graphviz_property_graph, typename,
## rewrite
average_rewrite

using Catlab
using Catlab.Theories
import Catlab.Theories: otimes, oplus, compose, ⊗, ⊕, ⋅, associate, associate_unit, Ob, Hom, dom, codom
using Catlab.Programs
using Catlab.CategoricalAlgebra
using Catlab.WiringDiagrams
using Catlab.WiringDiagrams.DirectedWiringDiagrams
using Catlab.ACSetInterface
using MLStyle
import Unicode

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
include("deca/Deca.jl")
include("learn/Learn.jl")

using .Deca

end
