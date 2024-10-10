using DiagrammaticEquations
using ACSets

export collected_incident

function collected_incident(d::ACSet, searches::AbstractVector, args...)

  isempty(searches) && error("Cannot have an empty search")

  query_result = mapreduce(vcat, searches) do search
    collected_incident(d, search, args...)
  end

  return unique!(query_result)
end

function collected_incident(d::ACSet, search, lookup_array)
  numof_channels = length(lookup_array)
  empty_outputchannels = fill(nothing, numof_channels)
  return collected_incident(d, search, lookup_array, empty_outputchannels)
end


function collected_incident(d::ACSet, search, lookup_array, output_array)
  length(lookup_array) == length(output_array) || error("Input and output channels are different lengths")
  isempty(lookup_array) && error("Cannot have an empty lookup")

  query_result = mapreduce(vcat, zip(lookup_array, output_array)) do (lookup, output)
    runincident_output_result(d, search, lookup, output)
  end

  return unique!(query_result)
end
  
function runincident_output_result(d::ACSet, search, lookup::Union{Symbol, AbstractVector{Symbol}}, output_channel::Union{Symbol, Nothing})
  index_result = incident(d, search, lookup)
  isnothing(output_channel) ? index_result : d[index_result, output_channel]
end
  
  
  