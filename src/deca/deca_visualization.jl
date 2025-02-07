### Drawing of Decapodes
import Catlab.Graphics.Graphviz
using Catlab.Graphics
using Catlab.Graphics.Graphviz
using Catlab.Graphs.PropertyGraphs
using Catlab.Graphs
using Catlab.Graphs.BasicGraphs

import LinearAlgebra: norm
using StatsBase
using Colors
using ColorSchemes
using ColorSchemeTools

# TODO: Change orientation to print 
# TODO: generalize ok??
"""    Graphics.to_graphviz(F::AbstractDecapode; directed = true, kw...)

Visualize the given Decapode through Graphviz. Ensure that you have called `using Catlab.Graphics` before-hand, and have a way of visualizing SVG files in your current environment.
"""
Graphics.to_graphviz(F::AbstractDecapode; directed = true, verbose = true, kw...) =
to_graphviz(GraphvizGraphs.to_graphviz_property_graph(F; typename, directed, verbose, kw...))

Decapode_edge_label(s::Symbol) = String(s)
Decapode_edge_label(s::Vector{Symbol}) = join(String.(s), "⋅")
Decapode_edge_label(s::String) = s
Decapode_edge_label(s::Vector{String}) = join(s, "⋅")

reg_to_sub = Dict("Form0"=>'₀', "Form1"=>"₁", "Form2"=>'₂', "Form3"=>'₃', 
                  "DualForm0"=>'₀', "DualForm1"=>"₁", "DualForm2"=>'₂', "DualForm3"=>'₃', 
                  "infer"=>'•', "Literal"=>'L', "Constant"=>'C', "Parameter"=>'P', )

toSub(digit::String) = get(reg_to_sub, digit, digit)

## TODO refactor
spacename(d, v) = begin
    t = d[v, :type]
    subscript = toSub(String(t))
    dom = startswith(String(t), "Dual") ? "Ω̃" : "Ω"
    # TODO: To get rid of omega for non-form types
    # dom = endswith(String(t), r"[0-9]") ? dom : ""
    return "$dom$subscript"
    # return t
end

varname(d, v, verbose) = begin 
  name =  "$(d[v, :name])"

  if(!verbose)
    cut_off_delim = findlast('_', name)
    if(!isnothing(cut_off_delim))
      name = name[cut_off_delim+1:end]
    end
  end

  return "$name:$(spacename(d, v))"
end

# a nice pastel scheme
const colors = ColorSchemes.cyclic_mygbm_30_95_c78_n256

function to_integer(x::RGB{Float64})
    (Int(floor(255*x.b)), Int(floor(255*x.g)), Int(floor(255*x.r)))
end

"""    proximate_color(color::RGB{Float64})::String

Given a RGB value, obtain the color word whose color is closest to the RGB value.

This is necessary to pass colors to GraphViz, which only accepts color words.
"""
function proximate_color(color::RGB{Float64}, 
        color_names::Dict{String, Tuple{Int64, Int64, Int64}}=Colors.color_names)
    integer_color = to_integer(color)
    normdiff = norm.([integer_color .- value for value ∈ values(color_names)])
    idx = first((1:length(normdiff))[normdiff .== minimum(normdiff)])
    # ^ `only` did not work as there were two minima. Need more principled way of picking!
    collect(keys(color_names))[idx]
end

const available_colors = unique(proximate_color.(colors))

"""    get_colors(d::SummationDecapode)::Dict{String, String}

Given a Decapode, we infer the name of the boxes in the cospan which defines it and associate to these boxes a random unique color from a list of colors.
"""
function get_colors(d::SummationDecapode)
    ns = collect(values(d.subparts.name.m))
    vals = String.(ns)
    paths = split.(vals, "_")
    # vectors with length > 1 are those which have been prefixed by a "_".
    spaces = first.(filter(p -> length(p) > 1, paths)) |> unique
    swatches = sample(available_colors, length(spaces), replace=false)
    Dict([space => swatches for (space, swatches) in zip(spaces, swatches)])
end


function labelcolor(s::Symbol, colordict::Dict{SubString{String}, T}) where T
    head = first(split(String(s), "_"))
    haskey(colordict, head) ? String(colordict[head]) : "white"
    # the default color is "white" because the default background for GraphViz visualizations in
    # AlgebraicJulia is white.
end

function labelcolor(s::String, colordict::Dict{SubString{String}, Symbol})
    head = first(split(s, "_"))
    haskey(colordict, head) ? String(colordict[head]) : "white"
end

# TODO: generalize ok??
function Catlab.Graphics.to_graphviz_property_graph(d::SummationDecapode; typename=spacename, directed = true, prog = "dot", node_attrs=Dict(), edge_attrs=Dict(), graph_attrs=Dict(), node_labels = true, verbose = true, kw...)
   
    colordict = get_colors(d)

    default_graph_attrs = Dict(:rankdir => "TB")
    default_edge_attrs = Dict()
    default_node_attrs = Dict(:shape => "oval")

    G = to_graphviz_property_graph(Catlab.Graphs.Graph(0); prog, node_labels,
      node_attrs = merge!(default_node_attrs, node_attrs),  
      edge_attrs = merge!(default_edge_attrs, edge_attrs),  
      graph_attrs = merge!(default_graph_attrs, graph_attrs))

    vids = map(parts(d, :Var)) do v
      vertex = add_vertex!(G, label=varname(d, v, verbose))
      set_vprop!(G, v, :fillcolor, labelcolor(subpart(d, v, :name), colordict))
      set_vprop!(G, v, :style, "filled")
      vertex
    end

    # Add entry and exit vertices and wires
    if(directed)
      tempin = add_vertex!(G, shape = "none", label = "")
      for state in infer_states(d)
        add_edge!(G, tempin, vids[state])
      end

      tempout = add_vertex!(G, shape = "none", label = "")
      for tvar in d[:incl]
        add_edge!(G, vids[tvar], tempout)
      end
    end

    map(parts(d, :Op1)) do op
      s, t = d[op, :src], d[op, :tgt]
      add_edge!(G, s, t, label=Decapode_edge_label(d[op,:op1]))
    end

    ## TODO: passing in typename, despite it being a variable, 
    map(parts(d, :Op2)) do op
      s, t = d[op, :proj1], d[op, :proj2]
      r = d[op, :res]
      # S, T, = typename(d, s), typename(d, t)
      # v = add_vertex!(G, label="$(S)×$(T)", shape="rectangle")
      v = add_vertex!(G, label="$(spacename(d, s))×$(spacename(d,t))", shape="rectangle")

      # If in directed mode, sources point into the projection field
      # Else, everything points out
      if(directed)
        add_edge!(G, s, v, label="π₁", style="dashed")
        add_edge!(G, t, v, label="π₂", style="dashed")
      else
        add_edge!(G, v, s, label="π₁", style="dashed")
        add_edge!(G, v, t, label="π₂", style="dashed")
      end
      add_edge!(G, v, r, label=Decapode_edge_label(d[op, :op2]))
    end

    findvid(G, d, v) = incident(G.graph, [Dict{Symbol, Any}(:label=>varname(d, v, verbose))], :vprops)
    white_nodes = map(parts(d, :Σ)) do s
        v = add_vertex!(G, label="Σ$s", shape="circle")
        u = d[s, :sum]
        add_edge!(G, v, u, label="+")
        return v
    end
    for e in parts(d, :Summand)
        # If directed, point summands into the sum
        # Else, everything points outward
        if(directed)
          e = add_edge!(G, d[e, :summand], white_nodes[d[e, :summation]], style="dashed")
        else
          e = add_edge!(G, white_nodes[d[e, :summation]], d[e, :summand], style="dashed")
        end
    end
    return G
end


