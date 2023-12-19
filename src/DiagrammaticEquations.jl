""" Some description of ths package
"""
module DiagrammaticEquations

export 
DerivOp, append_dot, normalize_unicode, infer_states,
# Deca
op1_res_rules_1D,
op2_res_rules_1D,
op1_res_rules_2D,
op2_res_rules_2D,
op1_inf_rules_1D,
op2_inf_rules_1D,
op1_inf_rules_2D,
op2_inf_rules_2D,
recursive_delete_parents, findname, spacename, varname, unicode!, apply_inference_rule_op1!, apply_inference_rule_op2!,
## collages
Collage, collate,
## composition
oapply, unique_by, unique_by!, OpenSummationDecapodeOb, OpenSummationDecapode, Open,
## acset
SchDecapode, SchNamedDecapode, AbstractDecapode, AbstractNamedDecapode, NamedDecapode, SummationDecapode,
contract_operators, add_constant!, add_parameter, vec_to_dec!, infer_types!,
fill_names!,  dot_rename!, expand_operators, infer_state_names, recognize_types,
resolve_overloads!, replace_names!,
## language
@decapode, Term, parse_decapode, term, Eq, DecaExpr,
# ~~~~~
Plus, AppCirc1, AppCirc2, Var, Tan, App1, App2, Judgment, 
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

# using .Deca
# import .Deca: to_graphviz_property_graph 


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

include("deca/Deca.jl")
include("learn/Learn.jl")

end
