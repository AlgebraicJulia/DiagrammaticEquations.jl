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

#const colors = ColorSchemes.cyclic_mygbm_30_95_c78_n256 # A nice pastel scheme
# Approximate the above color-scheme with a piecewise polynomial in RGB space.
const corners = [[0.90, 0.90, 0.10],
                 [0.20, 0.70, 0.15],
                 [0.20, 0.10, 0.90],
                 [0.90, 0.35, 0.95]]
const poly_line = [[1,2], [2,3], [3,4], [4,1]]

"""    get_colors(d::SummationDecapode)

Given a Decapode, we infer the name of the boxes in the cospan which defines it and associate to these boxes a random unique color from a list of colors.
"""
function get_colors(d::SummationDecapode)
    # Vectors with length > 1 are those which have been prefixed by a "_".
    names = String.(d[:name])
    paths = split.(names, "_")
    spaces = first.(filter(p -> length(p) > 1, paths))
    setdiff!(spaces, ["sum", ""])
    swatches = map(spaces) do space
      line = corners[sample(poly_line)]
      point_on_line = rand()
      swatch = Colors.RGB((line[1]*point_on_line + line[2]*(1-point_on_line))...)
    end
    Dict([space => "#" * Colors.hex(swatch) for (space, swatch) in zip(spaces, swatches)])
end

function labelcolor(s::T, colordict::Dict{SubString{String}, U}) where {T, U}
    head = first(split(String(s), "_"))
    String(get(colordict, head, "#ffffff"))
end

function Catlab.Graphics.to_graphviz_property_graph(d::SummationDecapode; typename=spacename, directed = true, prog = "dot", node_attrs=Dict(), edge_attrs=Dict(), graph_attrs=Dict(), node_labels = true, verbose = true, color = false, kw...)
    default_graph_attrs = Dict(:rankdir => "TB")
    default_edge_attrs = Dict()
    default_node_attrs = Dict(:shape => "oval")

    G = to_graphviz_property_graph(Catlab.Graphs.Graph(0); prog, node_labels,
      node_attrs = merge!(default_node_attrs, node_attrs),  
      edge_attrs = merge!(default_edge_attrs, edge_attrs),  
      graph_attrs = merge!(default_graph_attrs, graph_attrs))

    colordict = color ? get_colors(d) : Dict{SubString{String}, Symbol}()

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


