module Util

using Reexport

include("HashColor.jl")
include("Plotting.jl")
include("Dtrys.jl")

@reexport using .HashColor
@reexport using .Plotting
@reexport using .Dtrys

end
