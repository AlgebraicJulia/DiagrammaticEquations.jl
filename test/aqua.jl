using Aqua, DiagrammaticEquations
@testset "Code quality (Aqua.jl)" begin
    Aqua.test_all(DiagrammaticEquations,
        ambiguities=false,
        undefined_exports=false
    )
end
