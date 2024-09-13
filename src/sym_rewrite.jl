using DiagrammaticEquations
using SymbolicUtils
using MLStyle

test_space = Space(:X, 2)

test_type = Form(0, false, test_space)

Heat = @decapode begin
  C::Form0
  D::Constant
  ∂ₜ(C) == D*Δ(d(C))
end

infer_types!(Heat)
resolve_overloads!(Heat)

function isform_zero(x)
  getmetadata(x, Sort).dim == 0
end

function isform_one(x)
  getmetadata(x, Sort).dim == 1
end

function isform_two(x)
  getmetadata(x, Sort).dim == 2
end

@syms Δ(x) d(x) ⋆(x) Δ₀(x) Δ₁(x) Δ₂(x) d₀(x)

lap_0_convert = @rule Δ₀(~x) => Δ(~x)
lap_1_convert = @rule Δ₁(~x) => Δ(~x)
lap_2_convert = @rule Δ₂(~x) => Δ(~x)

d_0_convert = @rule d₀(~x) => d(~x)

overloaders = [lap_0_convert, lap_1_convert, lap_2_convert, d_0_convert]

lap_0_rule = @rule Δ(~x::(isform_zero)) => ⋆(d(⋆(d(~x))))
lap_1_rule = @rule Δ(~x::(isform_one)) => d(⋆(d(⋆(~x)))) + ⋆(d(⋆(d(~x))))
lap_2_rule = @rule Δ(~x::(isform_two)) => d(⋆(d(⋆(~x))))

openers = [lap_0_rule, lap_1_rule, lap_2_rule]

heat_exprs = extract_symexprs(Heat)

rewriter = SymbolicUtils.Postwalk(
            SymbolicUtils.Fixpoint(SymbolicUtils.Chain(vcat(overloaders, openers))))

res_exprs =  apply_rewrites(Heat, rewriter)

merge_exprs = merge_equations(Heat, res_exprs)

optm_dd_0 = @rule d(d(~x)) => 0
star_0 = @rule ⋆(0) => 0
d_0 = @rule d(0) => 0

optm_rewriter = SymbolicUtils.Postwalk(
  SymbolicUtils.Fixpoint(SymbolicUtils.Chain([optm_dd_0, star_0, d_0])))

res_merge_exprs = map(optm_rewriter, merge_exprs)

final_exprs = SymbolicUtils.Code.toexpr.(res_merge_exprs)
map(x -> x.args[1] = :(==), final_exprs)

to_decapode = quote
  C::Form0
  D::Constant
  Ċ::Form0
end

append!(to_decapode.args, final_exprs)

test = parse_decapode(to_decapode)
deca_test = SummationDecapode(test)
infer_types!(deca_test)
resolve_overloads!(deca_test)
