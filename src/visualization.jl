import Catlab.Graphics.Graphviz
using Catlab.Graphics
using Catlab.Graphics.Graphviz
using Catlab.Graphs.PropertyGraphs
using Catlab.Graphs
using Catlab.Graphs.BasicGraphs

#= reg_to_sub = Dict('0'=>'₀', '1'=>"₁", '2'=>'₂', '3'=>'₃', '4'=>'₄',
    '5'=>'₅', '6'=>'₆','7'=>'₇', '8'=>'₈', '9'=>'₉', 'r'=>'•', 'l'=>'L', ) =#


typename(d, v) = begin
  t = d[v, :type]
  return t 
end 

# function Catlab.Graphics.to_graphviz_property_graph(d::AbstractNamedDecapode; kw...)
#   to_graphviz_property_graph(d::AbstractNamedDecapode; typename=spacename, directed = true, kw...)
# end

# function Catlab.Graphics.to_graphviz_property_graph(d::SummationDecapode; kw...)
#   to_graphviz_property_graph(d::SummationDecapode; typename=spacename, directed = true, kw...)
# end
function Catlab.Graphics.to_graphviz_property_graph(d::AbstractNamedDecapode; typename=typename, directed=true, kw...)
    pg = PropertyGraph{Any}(;kw...)
    vids = map(parts(d, :Var)) do v
      add_vertex!(pg, label=varname(d,v))
    end

    # Add entry and exit vertices and wires
    if(directed)
      for state in infer_states(d)
        tempv = add_vertex!(pg)
        add_edge!(pg, tempv, vids[state])
      end
    end

    map(parts(d, :Op1)) do op
      s, t = d[op, :src], d[op, :tgt]
      add_edge!(pg, vids[s],vids[t], label=Decapode_edge_label(d[op,:op1]))
    end

    map(parts(d, :Op2)) do op
      s, t = d[op, :proj1], d[op, :proj2]
      r = d[op, :res]
      v = add_vertex!(pg, label="$(typename(d, s))×$(typename(d,t))", shape="rectangle")

      # If in directed mode, sources point into the projection field
      # Else, everything points out
      if(directed)
        add_edge!(pg, vids[s], v, label="π₁", style="dashed")
        add_edge!(pg, vids[t], v, label="π₂", style="dashed")
      else
        add_edge!(pg, v, vids[s], label="π₁", style="dashed")
        add_edge!(pg, v, vids[t], label="π₂", style="dashed")
      end
      add_edge!(pg, v, vids[r], label=Decapode_edge_label(d[op, :op2]))
    end

    return pg
end

draw_composition(deca) = to_graphviz(deca, box_labels=:name, junction_labels=:variable, prog="circo")

savevizsvg(g, fname::String) = open(fname, "w") do fp
  run_graphviz(fp, to_graphviz(to_graphviz_property_graph(nsdp)), prog="neato", format="svg")
end
