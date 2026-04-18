module CCLTest

using Test
using JSON3
using ACSets
using DiagrammaticEquations

@testset "Parsing CCL JSON" begin

    modeljson = JSON3.read("test/data/diffusivity_constant/model.json")
    parsed_model = CCLModel(modeljson)
    testmodel = UUIDLabeledGraph(parsed_model)

    @test testmodel[1, :elabel] == "d̃₁"
    @test testmodel[3, :elabel] == "-"

    diagramjson = JSON3.read("test/data/diffusivity_constant/diagram.json")
    parsed_diagram = CCLDiagram(diagramjson, testmodel)
    testdiagram = UUIDLabeledGraph(parsed_diagram)

    @test testdiagram[1, :elabel] == "d₀"

    pode = SummationDecapode(testdiagram)
    infer_types!(pode)

    # test/language:1052
    bespoke_op1_inf_rule = (src_type = :DualForm2, tgt_type = :DualForm2, op_names = [:any_scalar])
    infer_types!(pode, vcat([bespoke_op1_inf_rule], op1_inf_rules_1D), op2_inf_rules_1D)

end


end
