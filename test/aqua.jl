using Aqua, DiagrammaticEquations
@testset "Code quality (Aqua.jl)" begin
    Aqua.test_all(DiagrammaticEquations,
        deps_compat=false,
        ambiguities=false,
        undefined_exports=false
    )
end
