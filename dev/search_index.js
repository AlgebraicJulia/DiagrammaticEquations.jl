var documenterSearchIndex = {"docs":
[{"location":"generated/literate_example/","page":"Code Example","title":"Code Example","text":"EditURL = \"../../literate/literate_example.jl\"","category":"page"},{"location":"generated/literate_example/#Code-Example","page":"Code Example","title":"Code Example","text":"","category":"section"},{"location":"generated/literate_example/","page":"Code Example","title":"Code Example","text":"using DiagrammaticEquations","category":"page"},{"location":"api/#Library-Reference","page":"Library Reference","title":"Library Reference","text":"","category":"section"},{"location":"api/","page":"Library Reference","title":"Library Reference","text":"Modules = [DiagrammaticEquations, DiagrammaticEquations.Deca, DiagrammaticEquations.Learn]","category":"page"},{"location":"api/#DiagrammaticEquations.DiagrammaticEquations","page":"Library Reference","title":"DiagrammaticEquations.DiagrammaticEquations","text":"The DiagrammaticEquations module exports data structures which represent diagrammatic equations and functions which manipulate them.\n\n\n\n\n\n","category":"module"},{"location":"api/#DiagrammaticEquations.decapodeacset.SummationDecapode-Tuple{DecaExpr}","page":"Library Reference","title":"DiagrammaticEquations.decapodeacset.SummationDecapode","text":"function SummationDecapode(e::DecaExpr)\n\nTakes a DecaExpr and returns a SummationDecapode ACSet.\n\n\n\n\n\n","category":"method"},{"location":"api/#Catlab.WiringDiagrams.WiringDiagramAlgebras.oapply-Union{Tuple{D}, Tuple{Catlab.Programs.RelationalPrograms.RelationDiagram, Vector{D}}} where D<:(Catlab.CategoricalAlgebra.StructuredCospans.StructuredMulticospan{Catlab.CategoricalAlgebra.StructuredCospans.DiscreteACSet{ACSets.DenseACSets.AnonACSet{ACSets.Schemas.TypeLevelBasicSchema{Symbol, Tuple{:Var}, Tuple{}, Tuple{:Type, :Operator, :Name}, Tuple{(:type, :Var, :Type), (:name, :Var, :Name)}}, Tuple{Type, Operator, Name}, @NamedTuple{Var::ACSets.DenseACSets.IntParts, Type::ACSets.DenseACSets.IntParts, Operator::ACSets.DenseACSets.IntParts, Name::ACSets.DenseACSets.IntParts}, @NamedTuple{type::ACSets.ColumnImplementations.DenseColumn{Union{ACSets.ColumnImplementations.AttrVar, Type}, Array{Union{ACSets.ColumnImplementations.AttrVar, Type}, 1}}, name::ACSets.ColumnImplementations.DenseColumn{Union{ACSets.ColumnImplementations.AttrVar, Name}, Array{Union{ACSets.ColumnImplementations.AttrVar, Name}, 1}}}, ACSets.DenseACSets.IntParts}, SummationDecapode{Type, Operator, Name}}} where {Type, Operator, Name})","page":"Library Reference","title":"Catlab.WiringDiagrams.WiringDiagramAlgebras.oapply","text":"function oapply(relation::RelationDiagram, podes::Vector{D}) where {D<:OpenSummationDecapode}\n\nCompose a list of Decapodes as specified by the given relation diagram.\n\nThe Decapodes must be given in the same order as they were specified in the relation.\n\nState variables (such as the (C,V) given in the head of the following @relation) do not affect the result of a composition.\n\nExamples\n\njulia> compose_diff_adv = @relation (C,V) begin\n  diffusion(C, ϕ₁)\n  advection(C, ϕ₂, V)\n  superposition(ϕ₁, ϕ₂, ϕ, C)\nend;\n\njulia> oapply(compose_diff_adv, [(Diffusion, [:C, :ϕ]),\n  (Advection, [:C, :ϕ, :V]), (Superposition, [:ϕ₁, :ϕ₂, :ϕ, :C])]);\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.Decapode-Tuple{DecaExpr}","page":"Library Reference","title":"DiagrammaticEquations.Decapode","text":"function Decapode(e::DecaExpr)\n\nTakes a DecaExpr and returns a Decapode ACSet.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.Open-Union{Tuple{V}, Tuple{U}, Tuple{T}, Tuple{SummationDecapode{T, U, V}, AbstractVector{Symbol}}} where {T, U, V}","page":"Library Reference","title":"DiagrammaticEquations.Open","text":"Open(d::SummationDecapode{T,U,V}, names::AbstractVector{Symbol}) where {T,U,V}\n\ncreates an OpenSummationDecapode based on named variables rather than variable indices.  See AlgebraicPetri.jl's Open for the analogous verion for LabelledReactionNetworks.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.average_rewrite-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.average_rewrite","text":"function average_rewrite(deca_source::SummationDecapode)\n\nCompute each quantitity in the given Decapode by the average of all computation paths leading to that node.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.collate-NTuple{4, Any}","page":"Library Reference","title":"DiagrammaticEquations.collate","text":"function collate(equations, boundaries, uwd, symbols)\n\nCreate a collage of two Decapodes that simulates with boundary conditions. ```\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.contract_operators-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.contract_operators","text":"function contract_operators(d::SummationDecapode; allowable_ops::Set{Symbol} = Set{Symbol}())\n\nFind chains of Op1s in the given Decapode, and replace them with a single Op1 with a vector of function names. After this process, all Vars that are not a part of any computation are removed.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.dot_rename!-Tuple{AbstractNamedDecapode}","page":"Library Reference","title":"DiagrammaticEquations.dot_rename!","text":"dot_rename!(d::AbstractNamedDecapode)\n\nRename tangent variables by their depending variable appended with a dot. e.g. If D == ∂ₜ(C), then rename D to Ċ.\n\nIf a tangent variable updates multiple vars, choose one arbitrarily. e.g. If D == ∂ₜ(C) and D == ∂ₜ(B), then rename D to either Ċ or B ̇.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.expand_operators-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.expand_operators","text":"function expand_operators(d::SummationDecapode)\n\nFind operations that are compositions, and expand them with intermediate variables.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.fill_names!-Tuple{AbstractNamedDecapode}","page":"Library Reference","title":"DiagrammaticEquations.fill_names!","text":"function fill_names!(d::AbstractNamedDecapode; lead_symbol::Symbol = Symbol(\"•\"))\n\nProvide a variable name to all the variables that don't have names.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.find_chains-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.find_chains","text":"function findchains(d::SummationDecapode; allowableops::Set{Symbol} = Set{Symbol}())\n\nFind chains of Op1s in the given Decapode. A chain ends when the target of the last Op1 is part of an Op2 or sum, or is a target of multiple Op1s. Only operators with names included in the  allowable_ops set are allowed to be contracted. If the set is empty then all operators are allowed.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.find_dep_and_order-Tuple{AbstractNamedDecapode}","page":"Library Reference","title":"DiagrammaticEquations.find_dep_and_order","text":"find_dep_and_order(d::AbstractNamedDecapode)\n\nFind the order of each tangent variable in the Decapode, and the index of the variable that it is dependent on. Returns a tuple of (dep, order), both of which respecting the order in which incident(d, :∂ₜ, :op1) returns Vars.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.find_tgts_of_many_ops-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.find_tgts_of_many_ops","text":"function find_tgts_of_many_ops(d::SummationDecapode)\n\nSearches SummationDecapode, d, for all Vars which have two or more distinct operations leading into the same variable.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.find_variable_mapping-Tuple{Any, Any}","page":"Library Reference","title":"DiagrammaticEquations.find_variable_mapping","text":"function find_variable_mapping(deca_source, deca_tgt)\n\nReturns array of match on variables between from a Decapode source to a target, also returns if mapping is valid WARNING: This assumes that variable names are unique. If variable names are not unique or do not exist,  corrsponding mapping value is set to 0.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.get_preprocess_indices-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.get_preprocess_indices","text":"function get_preprocess_indices(deca_source::SummationDecapode)\n\nSearches SummationDecapode, deca_source, for all Vars which are valid for average rewriting preprocessing. Namely this just includes all op2 and summation operations. Returns two arrays, first is  array of valid Op2 ids, second is array of valid Σ ids.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.get_valid_op1s-Tuple{SummationDecapode, Any}","page":"Library Reference","title":"DiagrammaticEquations.get_valid_op1s","text":"function get_valid_op1s(deca_source::SummationDecapode, varID)\n\nSearches SummationDecapode, deca_source, at the request varID and returns all op1s which are allowed to be averaged. Returns an array of indices of valid op1 sources.\n\nNamely this is meant to exclude ∂ₜ from being included in an average.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.infer_types!-Tuple{SummationDecapode, Vector{@NamedTuple{src_type::Symbol, tgt_type::Symbol, op_names::Vector{Symbol}}}, Vector{@NamedTuple{proj1_type::Symbol, proj2_type::Symbol, res_type::Symbol, op_names::Vector{Symbol}}}}","page":"Library Reference","title":"DiagrammaticEquations.infer_types!","text":"function infer_types!(d::SummationDecapode, op1_rules::Vector{NamedTuple{(:src_type, :tgt_type, :replacement_type, :op), NTuple{4, Symbol}}})\n\nInfer types of Vars given rules wherein one type is known and the other not.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.is_tgt_of_many_ops-Tuple{SummationDecapode, Any}","page":"Library Reference","title":"DiagrammaticEquations.is_tgt_of_many_ops","text":"function is_tgt_of_many_ops(d::SummationDecapode, var)\n\nReturn true if there are two or more distinct operations leading into Var var (not counting ∂ₜ).\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.preprocess_average_rewrite-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.preprocess_average_rewrite","text":"function preprocess_average_rewrite(deca_source::SummationDecapode)\n\nPreprocesses SummationDecapode, deca_source, for easier average  rewriting later on. Specifically, all op2 and summation results are stored in variables called \"Temp\" and results are then passed off to  their original result along an op1 called \"temp\". This \"temp\" operation is equivalent to an identity function.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.process_average_rewrite-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.process_average_rewrite","text":"function process_average_rewrite(deca_source::SummationDecapode)\n\nRewrites SummationDecapode, deca_source, by including averages of redundent operations. While this function only searches for op1s to match on, because of preprocessing, this indirectly includes op2  and summations in the final result.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.remove_neighborless_vars!-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.remove_neighborless_vars!","text":"function remove_neighborless_vars!(d::SummationDecapode)\n\nRemove all Vars from the given Decapode that are not part of any computation.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.resolve_overloads!-Tuple{SummationDecapode, Vector{@NamedTuple{src_type::Symbol, tgt_type::Symbol, resolved_name::Symbol, op::Symbol}}, Vector{@NamedTuple{proj1_type::Symbol, proj2_type::Symbol, res_type::Symbol, resolved_name::Symbol, op::Symbol}}}","page":"Library Reference","title":"DiagrammaticEquations.resolve_overloads!","text":"function resolve_overloads!(d::SummationDecapode, op1_rules::Vector{NamedTuple{(:src_type, :tgt_type, :resolved_name, :op), NTuple{4, Symbol}}})\n\nResolve function overloads based on types of src and tgt.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.type_check_Decapodes_composition-Union{Tuple{D}, Tuple{Catlab.Programs.RelationalPrograms.RelationDiagram, Vector{D}}} where D<:(Catlab.CategoricalAlgebra.StructuredCospans.StructuredMulticospan{Catlab.CategoricalAlgebra.StructuredCospans.DiscreteACSet{ACSets.DenseACSets.AnonACSet{ACSets.Schemas.TypeLevelBasicSchema{Symbol, Tuple{:Var}, Tuple{}, Tuple{:Type, :Operator, :Name}, Tuple{(:type, :Var, :Type), (:name, :Var, :Name)}}, Tuple{Type, Operator, Name}, @NamedTuple{Var::ACSets.DenseACSets.IntParts, Type::ACSets.DenseACSets.IntParts, Operator::ACSets.DenseACSets.IntParts, Name::ACSets.DenseACSets.IntParts}, @NamedTuple{type::ACSets.ColumnImplementations.DenseColumn{Union{ACSets.ColumnImplementations.AttrVar, Type}, Array{Union{ACSets.ColumnImplementations.AttrVar, Type}, 1}}, name::ACSets.ColumnImplementations.DenseColumn{Union{ACSets.ColumnImplementations.AttrVar, Name}, Array{Union{ACSets.ColumnImplementations.AttrVar, Name}, 1}}}, ACSets.DenseACSets.IntParts}, SummationDecapode{Type, Operator, Name}}} where {Type, Operator, Name})","page":"Library Reference","title":"DiagrammaticEquations.type_check_Decapodes_composition","text":"function type_check_Decapodes_composition(relation::RelationDiagram, decs::Vector{OpenSummationDecapode})\n\nCheck that the types of all Vars connected by the same junction match.\n\nThis function only throws an error on the first type mismatch found.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.unique_by!-Tuple{Any, Symbol, Vector{Symbol}}","page":"Library Reference","title":"DiagrammaticEquations.unique_by!","text":"function unique_by!(acset, column_names::Vector{Symbol})\n\nGiven column names from the same table, remove duplicate rows.\n\nWARNING: This function does not check if other tables index into the one given. Removal of rows is performed with prejudice.\n\nSee also: unique_by.\n\nExamples\n\njulia> unique_by!(parallel_arrows(Graph, 123), :E, [:src,:tgt]) == parallel_arrows(Graph, 1)\ntrue\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.unique_by-Tuple{Any, Symbol, Vector{Symbol}}","page":"Library Reference","title":"DiagrammaticEquations.unique_by","text":"function unique_by(acset, column_names::Vector{Symbol})\n\nGiven column names from the same table, return a copy of the acset with duplicate rows removed. Removal of rows is performed with prejudice.\n\nWARNING: This function does not check if other tables index into the one given. Removal of rows is performed with prejudice.\n\nSee also: unique_by!.\n\nExamples\n\njulia> unique_by(parallel_arrows(Graph, 123), :E, [:src,:tgt]) == parallel_arrows(Graph, 1)\ntrue\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.@decapode-Tuple{Any}","page":"Library Reference","title":"DiagrammaticEquations.@decapode","text":"macro decapode(e)\n\nConstruct a SummationDecapode using the Decapode Domain-Specific Language.\n\n\n\n\n\n","category":"macro"},{"location":"api/#DiagrammaticEquations.Deca.op1_inf_rules_1D","page":"Library Reference","title":"DiagrammaticEquations.Deca.op1_inf_rules_1D","text":"These are the default rules used to do type inference in the 1D exterior calculus.\n\n\n\n\n\n","category":"constant"},{"location":"api/#DiagrammaticEquations.Deca.op1_inf_rules_2D","page":"Library Reference","title":"DiagrammaticEquations.Deca.op1_inf_rules_2D","text":"These are the default rules used to do type inference in the 2D exterior calculus.\n\n\n\n\n\n","category":"constant"},{"location":"api/#DiagrammaticEquations.Deca.op1_res_rules_1D","page":"Library Reference","title":"DiagrammaticEquations.Deca.op1_res_rules_1D","text":"These are the default rules used to do function resolution in the 1D exterior calculus.\n\n\n\n\n\n","category":"constant"},{"location":"api/#DiagrammaticEquations.Deca.op1_res_rules_2D","page":"Library Reference","title":"DiagrammaticEquations.Deca.op1_res_rules_2D","text":"These are the default rules used to do function resolution in the 2D exterior calculus.\n\n\n\n\n\n","category":"constant"},{"location":"api/#Catlab.Graphics.GraphvizGraphs.to_graphviz-Tuple{AbstractDecapode}","page":"Library Reference","title":"Catlab.Graphics.GraphvizGraphs.to_graphviz","text":"Graphics.to_graphviz(F::AbstractDecapode; directed = true, kw...)\n\nVisualize the given Decapode through Graphviz. Ensure that you have called using Catlab.Graphics before-hand, and have a way of visualizing SVG files in your current environment.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.Deca.recursive_delete_parents-Tuple{SummationDecapode, Vector{Int64}}","page":"Library Reference","title":"DiagrammaticEquations.Deca.recursive_delete_parents","text":"function recursive_delete_parents!(d::SummationDecapode, to_delete::Vector{Int64})\n\nDelete the given nodes and their parents in the Decapode, recursively.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.Deca.unicode!-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.Deca.unicode!","text":"function unicode!(d::SummationDecapode)\n\nReplace ASCII operators with their Unicode equivalents.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.Deca.vec_to_dec!-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.Deca.vec_to_dec!","text":"function vec_to_dec!(d::SummationDecapode)\n\nReplace Vector Calculus operators with Discrete Exterior Calculus equivalents.\n\n\n\n\n\n","category":"method"},{"location":"api/#DiagrammaticEquations.resolve_overloads!-Tuple{SummationDecapode}","page":"Library Reference","title":"DiagrammaticEquations.resolve_overloads!","text":"function resolve_overloads!(d::SummationDecapode)\n\nResolve function overloads based on types of src and tgt.\n\n\n\n\n\n","category":"method"},{"location":"equations/#Simple-Equations","page":"Equations","title":"Simple Equations","text":"","category":"section"},{"location":"equations/","page":"Equations","title":"Equations","text":"This tutorial shows how to use Decapodes to represent simple equations. These aren't using any of the Discrete Exterior Calculus or CombinatorialSpaces features of Decapodes. They just are a reference for how to build equations with the @decapodes macro and see how they are stored as ACSets.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"using Catlab\nusing Catlab.Graphics\nusing DiagrammaticEquations\nusing DiagrammaticEquations.Deca","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"The harmonic oscillator can be written in Decapodes in at least three different ways.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"oscillator = @decapode begin\n  X::Form0\n  V::Form0\n\n  ∂ₜ(X) == V\n  ∂ₜ(V) == -k(X)\nend","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"The default representation is a tabular output as an ACSet. The tables are Var for storing variables (X) and their types (Form0). TVar for identifying a subset of variables that are the tangent variables of the dynamics (Ẋ). The unary operators are stored in Op1 and binary operators stored in Op2. If a table is empty, it doesn't get printed.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"Even though a diagrammatic equation is like a graph, there are no edge tables, because the arity (number of inputs) and coarity (number of outputs) is baked into the operator definitions.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"You can also see the output as a directed graph. The input arrows point to the state variables of the system and the output variables point from the tangent variables. You can see that I have done the differential degree reduction from  x'' = -kx by introducing a velocity term v. Decapodes has some support for derivatives in the visualization layer, so it knows that dX/dt should be called Ẋ and that dẊ/dt should be called Ẋ̇.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"to_graphviz(oscillator)","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"In the previous example, we viewed negation and transformation by k as operators. Notice that k appears as an edge in the graph and not as a vertex. You can also use a 2 argument function like multiplication (*). With a constant value for k::Constant. In this case you will see k enter the diagram as a vertex and multiplication with * as a binary operator.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"oscillator = @decapode begin\n  X::Form0\n  V::Form0\n\n  k::Constant\n\n  ∂ₜ(X) == V\n  ∂ₜ(V) == -k*(X)\nend","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"This gives you a different graphical representation as well. Now we have the cartesian product objects which represent a tupling of two values.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"to_graphviz(oscillator)","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"You can also represent negation as a multiplication by a literal -1.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"oscillator = @decapode begin\n  X::Form0\n  V::Form0\n\n  k::Constant\n\n  ∂ₜ(X) == V\n  ∂ₜ(V) == -1*k*(X)\nend","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"Notice that the type bubble for the literal one is ΩL. This means that it is a literal. The literal is also used as the variable name.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"infer_types!(oscillator)\nto_graphviz(oscillator)","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"We can allow the material properties to vary over time by changing Constant to Parameter. This is how we tell the simulator that it needs to call k(t) at each time step to get the updated value for k or if it can just reuse that constant k from the initial time step.","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"oscillator = @decapode begin\n  X::Form0\n  V::Form0\n\n  k::Parameter\n\n  ∂ₜ(X) == V\n  ∂ₜ(V) == -1*k*(X)\nend","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"infer_types!(oscillator)\nto_graphviz(oscillator)","category":"page"},{"location":"equations/","page":"Equations","title":"Equations","text":"Often you will have a linear material where you are scaling by a constant, and a nonlinear version of that material where that scaling is replaced by a generic nonlinear function. This is why we allow Decapodes to represent both of these types of equations.","category":"page"},{"location":"#DiagrammaticEquations.jl","page":"DiagrammaticEquations.jl","title":"DiagrammaticEquations.jl","text":"","category":"section"},{"location":"","page":"DiagrammaticEquations.jl","title":"DiagrammaticEquations.jl","text":"CurrentModule = DiagrammaticEquations","category":"page"},{"location":"","page":"DiagrammaticEquations.jl","title":"DiagrammaticEquations.jl","text":"DiagrammaticEquations.jl is a Julia library implementing category-theoretic reasoning about systems of equations with diagrams. This package was extractedd from Decapodes.jl to decouple the representation of systems of equations from the solution of those equations. DiagrammaticEquations will grow to support many flavors of equations, starting with the discrete exterior calculus equations from Decapodes.","category":"page"}]
}
