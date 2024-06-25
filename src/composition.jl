import Catlab.CategoricalAlgebra: apex, feet, legs
import Catlab.WiringDiagrams: oapply

# ACSet manipulation helper functions
# -----------------------------------

"""    function unique_by!(acset, column_names::Vector{Symbol})

Given column names from the same table, remove duplicate rows.

WARNING: This function does not check if other tables index into the one given.
Removal of rows is performed with prejudice.

See also: [`unique_by`](@ref).

# Examples
```julia-repl
julia> unique_by!(parallel_arrows(Graph, 123), :E, [:src,:tgt]) == parallel_arrows(Graph, 1)
true
```
"""
function unique_by!(acset, table::Symbol, columns::Vector{Symbol})
  # Note: Declarative CT methods are prefered to imperative index arithmetic.
  rows = mapreduce(x -> acset[x], hcat, columns)
  rem_parts!(acset, table,
    setdiff(parts(acset, table),
      unique(i -> rows[i,:], eachindex(eachrow(rows)))))
  return acset
end

"""    function unique_by(acset, column_names::Vector{Symbol})

Given column names from the same table, return a copy of the acset with
duplicate rows removed. Removal of rows is performed with prejudice.

WARNING: This function does not check if other tables index into the one given.
Removal of rows is performed with prejudice.

See also: [`unique_by!`](@ref).

# Examples
```julia-repl
julia> unique_by(parallel_arrows(Graph, 123), :E, [:src,:tgt]) == parallel_arrows(Graph, 1)
true
```
"""
function unique_by(acset, table::Symbol, columns::Vector{Symbol})
  acset_copy = copy(acset)
  unique_by!(acset_copy, table, columns)
end

# Operadic composition
# --------------------

OpenSummationDecapodeOb, OpenSummationDecapode = OpenACSetTypes(SummationDecapode, :Var)

#FIXME: why can't we just add a constructor for OpenSummationDecapode
"""    Open(d::SummationDecapode{T,U,V}, names::AbstractVector{Symbol}) where {T,U,V}

creates an OpenSummationDecapode based on named variables rather than variable indices. 
See AlgebraicPetri.jl's Open for the analogous verion for LabelledReactionNetworks.
"""
function Open(d::SummationDecapode{T,U,V}, names::AbstractVector{Symbol}) where {T,U,V}
  legs = map(names) do name
    FinFunction(incident(d, name, :name), nparts(d, :Var))
  end
  OpenSummationDecapode{T,U,V}(d, legs...)
end

apex(Decapode::OpenSummationDecapode) = apex(Decapode.cospan)
legs(Decapode::OpenSummationDecapode) = legs(Decapode.cospan)
feet(Decapode::OpenSummationDecapode) = Decapode.feet

"""    function type_check_Decapodes_composition(relation::RelationDiagram, decs::Vector{OpenSummationDecapode})

Check that the types of all Vars connected by the same junction match.

This function only throws an error on the first type mismatch found.
"""
function type_check_Decapodes_composition(relation::RelationDiagram, decs::Vector{D}) where {D<:OpenSummationDecapode}
  r = relation
  types = [flatten([f[:type] for f in feet(d)]) for d in decs]
  return all(map(junctions(r)) do j
    ports = incident(r, j, :junction)
    ts = types[ports]
    all(ts[1] .== ts)
  end)
end


#function oapply_rename(relation::RelationDiagram, Decapodes::Vector{OpenSummationDecapode})
function oapply_rename(relation::RelationDiagram, Decapodes::Vector{D}) where D<:OpenSummationDecapode
  r = relation
  # The deepcopy. is necessary because if multiple Decapodes in the vector are
  # OpenPodes of the same SummationDecapode, their apex will point to the same
  # spot in memory. This interferes with renaming.
  Decapodes_vars = deepcopy.(collect(map(apex, Decapodes)))
  # FIXME: in this line, you should cast the SummationDecapode{S,T, Symbol} to SummationDecapode{S,T,Vector{Symbol}}
  # This will allow you to return namespace scoped variables.
  # Check that the number of Decapodes given matches the number of boxes in the
  # relation.
  num_boxes = nboxes(r)
  num_Decapodes = length(Decapodes_vars)
  # TODO: Should this be an ArgumentError?
  num_boxes == num_Decapodes || error(
    "$(num_boxes) Decapodes were specified in the relation but only "*
    "$(num_Decapodes) were given.")

  # Check that the number of variables given in the relation is the same as the
  # number of symbols in the corresponding vector of Vars.
  # TODO: Should this be an ArgumentError?
  for b ∈ boxes(r)
    # Note: This only returns the first length mismatch found.
    num_junctions = length(incident(r, b, :box))
    num_symbols = length(feet(Decapodes[b]))
    num_junctions == num_symbols || let Decapode_name = r[b,  :name]
      error("Component $(Decapode_name) expects $(num_junctions) interface variables, but number of feet is $(num_symbols).")
    end
    # FIXME: this should also check the types of the feet.
  end

  # Determine the mapping of global ports to local ports. In a RelationDiagram,
  # this is baked into the order of rows in the Port table.
  # This is a column that one could hcat to the Port table.
  # local_ports = [lp for b=boxes(r) for lp=eachindex(ports(r, b))]

  # Check that types of variables connected by the same junction match.
  # type_check_Decapodes_composition(relation, Decapodes_vars) || error("Composition Doesn't Typecheck")

  # Do namespacing.
  # Append each Var name with the name @relation gave the Decapode.
  # FIXME: return a list of symbols here instead of underscore separated list.
  for b ∈ boxes(r)
    box_name = r[b, :name]
    for v ∈ parts(Decapodes_vars[b], :Var)
      if Decapodes_vars[b][v, :type] != :Literal
        var_name = Decapodes_vars[b][v, :name]
        Decapodes_vars[b][v, :name] = Symbol(box_name, '_', var_name)
      end
    end
  end

  # Write over the name fields to be what was specified by @relation. (oapply
  # cannot combine objects whose attributes are not equal.)

  newnames = map(boxes(r)) do b
    pode = Decapodes[b]
    ports = incident(r, b, :box)
    localnames = map(enumerate(ports)) do (i,p)
      localnamevec = feet(pode)[i][:name]
      length(localnamevec) == 1 || error("Decapode arguments to oapply do not support bundling. Each foot should have 1 vertex")
      localnamevec[1]
    end

    pode_vars = Decapodes_vars[b]
    map(zip(ports, localnames)) do (p, lname)
      # FIXME: this will break when we add proper namespacing
      # Note: only is not necessary but is a useful check the Decapode is
      # well-formed. If we ever want e.g. X:Form0 and X:Form1 in a single
      # Decapode, this will need refactoring.
      name = Symbol(r[b, :name], '_', lname)
      var = only(incident(pode_vars, name, :name))
      j = r[p, :junction]
      globalname = r[j, :variable]
      pode_vars[var, :name] = globalname
      return globalname
    end
  end

  newpodes = map(boxes(r)) do b
    Open(Decapodes_vars[b], newnames[b])
  end

  uwd = UndirectedWiringDiagram(0)
  copy_parts!(uwd, r)

  #return oapply(r, newpodes)
  return oapply(uwd, newpodes)
end

# Infinite loop:
"""    function oapply(relation::RelationDiagram, podes::Vector{D}) where {D<:OpenSummationDecapode}

Compose a list of Decapodes as specified by the given relation diagram.

The Decapodes must be given in the same order as they were specified in the
relation.

State variables (such as the (C,V) given in the head of the following
@relation) do not affect the result of a composition.

# Examples
```julia-repl
julia> compose_diff_adv = @relation (C,V) begin
  diffusion(C, ϕ₁)
  advection(C, ϕ₂, V)
  superposition(ϕ₁, ϕ₂, ϕ, C)
end;

julia> oapply(compose_diff_adv, [(Diffusion, [:C, :ϕ]),
  (Advection, [:C, :ϕ, :V]), (Superposition, [:ϕ₁, :ϕ₂, :ϕ, :C])]);
```
"""
oapply(r::RelationDiagram, podes::Vector{D}) where {D<:OpenSummationDecapode} =
  oapply_rename(r, podes)
#oapply(r::RelationDiagram, podes::Vector{D}) where {D<:OpenSummationDecapode} =
#  invoke(oapply,
#    Tuple{UndirectedWiringDiagram, Vector{<:StructuredMulticospan{L}} where L},
#    r, oapply_rename(r, podes))

#oapply(r::RelationDiagram, pode::OpenSummationDecapode) = oapply(r, [pode])
# Luke changed the above line to the below line, for e.g. the case: (Note H should be renamed to N.)
# r = @relation () begin
#   heat(N)
# end
# oapply(r, OpenPode(Heat, [:H]))

oapply(r::RelationDiagram, pode::OpenSummationDecapode) = oapply(r, [pode])

# Default composition
# -------------------

# This helper function finds elements which appear in an array more than once.
function find_duplicates(vs::Vector{T}) where T
  once, twice = Set{T}(), Set{T}()
  foreach(v -> v ∈ once ? push!(twice,v) : push!(once,v), vs)
  twice
end

# TODO: Upstream this to Catlab?
function construct_relation_diagram(boxes::Vector{Symbol}, junctions::Vector{Vector{Symbol}})
  tables = map(boxes, junctions) do b, j
    Expr(:call, b, j...)
  end
  quote @relation () begin $(tables...) end end |> eval
end

# TODO: Add a macro which provides names for boxes via the Symbol of the Decapode.
"""    function default_composition_diagram(podes::Vector{D}, names::Vector{Symbol}) where {D<:SummationDecapode}

Given a list of Decapodes and their names, return a composition diagram which assumes that variables sharing the same name ought to be composed.

No Literals are exposed. Use [`unique_lits!`](@ref) after composing.

Throw an error if any individual Decapode already contains a repeated name (except for Literals).

If `only_states_terminals` is `true`, only expose state and terminal variables. Defaults to `false`.

Note that composing immediately with [`oapply`](@ref) will fail if types do not match (e.g. (:infer, :Form0) or (:Form0, :Form1)).
"""
function default_composition_diagram(podes::Vector{D}, names::Vector{Symbol}, only_states_terminals=false) where {D<:SummationDecapode}
  non_lit_names = map(podes) do pode
    pode[findall(!=(:Literal), pode[:type]), :name]
  end
  for (nln, name) in zip(non_lit_names, names)
    allunique(nln) || error("Decapode $name contains repeated variable names: $(find_duplicates(nln)).")
  end
  if only_states_terminals
    foreach(non_lit_names, podes) do nln, pode
      outers = infer_state_names(pode) ∪ infer_terminal_names(pode)
      filter!(x -> x ∈ outers, nln)
    end
  end
  construct_relation_diagram(names, non_lit_names)
end

