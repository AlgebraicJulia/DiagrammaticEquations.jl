
#= reg_to_sub = Dict('0'=>'₀', '1'=>"₁", '2'=>'₂', '3'=>'₃', '4'=>'₄',
    '5'=>'₅', '6'=>'₆','7'=>'₇', '8'=>'₈', '9'=>'₉', 'r'=>'•', 'l'=>'L', ) =#

## TODO generalize ok??
reg_to_sub = Dict("Form0"=>'₀', "Form1"=>"₁", "Form2"=>'₂', "Form3"=>'₃', 
                  "DualForm0"=>'₀', "DualForm1"=>"₁", "DualForm2"=>'₂', "DualForm3"=>'₃', 
                  "infer"=>'•', "Literal"=>'L', "Constant"=>'C', "Parameter"=>'P', )

toSub(digit::String) = get(reg_to_sub, digit, digit)

## TODO refactor
typename(d, v) = begin
    t = d[v, :type]
    subscript = toSub(String(t))
    dom = startswith(String(t), "Dual") ? "Ω̃" : "Ω"
    # TODO: To get rid of omega for non-form types
    # dom = endswith(String(t), r"[0-9]") ? dom : ""
    return "$dom$subscript"
end
varname(d, v) = "$(d[v, :name]):$(typename(d, v))"


to_graphviz_property_graph(d::AbstractNamedDecapode; kw...) = 
to_graphviz_property_graph(d::AbstractNamedDecapode; typename=spacename, directed = true, kw...);

to_graphviz_property_graph(d::SummationDecapode; kw...) = 
to_graphviz_property_graph(d::SummationDecapode; typename=spacename, directed = true, kw...); 


