using Aqua, DiagrammaticEquations
@testset "Code quality (Aqua.jl)" begin
    # TODO: fix ambiguities
    Aqua.test_all(DiagrammaticEquations, ambiguities=false, undefined_exports=false, piracies=false)
end
