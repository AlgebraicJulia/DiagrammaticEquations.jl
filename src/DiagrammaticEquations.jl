""" Some description of ths package
"""
module DiagrammaticEquations

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

export 
# Deca
normalize_unicode, DerivOp, append_dot, recursive_delete_parents, findname,
op1_inf_rules_1D, op2_inf_rules_1D, op1_inf_rules_2D, op2_inf_rules_2D, op1_res_rules_1D, op2_res_rules_1D, op1_res_rules_2D, op2_res_rules_2D,
## collages
Collage, collate,
## composition
oapply, unique_by, unique_by!, OpenSummationDecapodeOb, OpenSummationDecapode, Open,
## acset
unicode!,
SchDecapode, SchNamedDecapode, AbstractDecapode, AbstractNamedDecapode, NamedDecapode, SummationDecapode,
contract_operators, add_constant!, add_parameter, vec_to_dec!, infer_types!,
fill_names!,  dot_rename!, expand_operators, infer_state_names, recognize_types,
resolve_overloads!,
## language
@decapode, Term, parse_decapode, term, Eq, DecaExpr,
# ~~~~~
Plus, AppCirc1, AppCirc2, Var, Tan, App1, App2, Judgment, 
## visualization
to_graphviz_property_graph,
## rewrite
average_rewrite

## TODO:
## generate schema from a _theory_
## from presentation in the theory determine what to slice over
## Pavel S., Hypergraph theory string diagram

include("acset.jl")
include("language.jl")
include("composition.jl")
include("collages.jl")
include("visualization.jl")
include("rewrite.jl")
include("deca/Deca.jl")

end
