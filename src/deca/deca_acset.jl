using ..DiagrammaticEquations

# TODO: You could write a method which auto-generates these rules given degree N.
"""
These are the default rules used to do type inference/function resolution in the 1D/2D exterior calculus.
"""
op1_operators = [
  # Rules for ∂ₜ
  Rule(:Form0, :Form0, :∂ₜ, [:dt]),
  Rule(:Form1, :Form1, :∂ₜ, [:dt]),
  Rule(:Form2, :Form2, :∂ₜ, [:dt]),
  Rule(:DualForm0, :DualForm0, :∂ₜ, [:dt]),
  Rule(:DualForm1, :DualForm1, :∂ₜ, [:dt]),
  Rule(:DualForm2, :DualForm2, :∂ₜ, [:dt]),

  # Rules for d.
  Rule(:Form1, :Form0, :d₀, [:d]),
  Rule(:Form2, :Form1, :d₁, [:d]),
  Rule(:DualForm1, :DualForm0, :dual_d₀, [:d, :d̃₀]),
  Rule(:DualForm2, :DualForm1, :dual_d₁, [:d, :d̃₁]),

  # Rules for δ
  Rule(:Form0, :Form1, :δ₁, [:δ, :codif]),
  Rule(:Form1, :Form2, :δ₂, [:δ, :codif]),

  # Rules for ♯
  Rule(:PVF, :Form1, :♯ᵖᵖ, [:♯]),
  Rule(:DVF, :DualForm1, :♯ᵈᵈ, [:♯]),
  # Rule(:DVF, :Form1, :♯ᵖᵈ, [:♯]),

  # Rules for ♭
  Rule(:Form1, :DVF, :♭ᵈᵖ, [:♭]),

  # Rules for Δ
  Rule(:Form0, :Form0, :Δ₀, [:Δ, :∇², :lapl]),
  Rule(:Form1, :Form1, :Δ₁, [:Δ, :∇², :lapl]),
  Rule(:Form2, :Form2, :Δ₂, [:Δ, :∇², :lapl]),

  # Rules for Δᵈ
  Rule(:DualForm0, :DualForm0, :Δᵈ₀, [:Δ, :∇², :lapl]),
  Rule(:DualForm1, :DualForm1, :Δᵈ₁, [:Δ, :∇², :lapl]),

  Rule(:Form0, :Form0, :(-), [:neg]),
  Rule(:Form1, :Form1, :(-), [:neg]),
  Rule(:Form2, :Form2, :(-), [:neg]),
  Rule(:DualForm0, :DualForm0, :(-), [:neg]),
  Rule(:DualForm1, :DualForm1, :(-), [:neg]),
  Rule(:DualForm2, :DualForm2, :(-), [:neg]),

  # Rules for the averaging operator
  Rule(:Form1, :Form0, :avg₀₁, [:avg_01]),

  Rule(:Form0, :PVF, :norm, [:mag]),
  Rule(:DualForm0, :DVF, :norm, [:mag])
]

op1_1D_bound_operators = [

  # Rules for ⋆
  Rule(:DualForm1, :Form0, :⋆₀, [:★, :⋆, :star]),
  Rule(:DualForm0, :Form1, :⋆₁, [:★, :⋆, :star]),
  Rule(:Form0, :DualForm1, :⋆₀⁻¹, [:★, :⋆, :star, :star_inv]),
  Rule(:Form1, :DualForm0, :⋆₁⁻¹, [:★, :⋆, :star, :star_inv])
]

op1_2D_bound_operators = [

  # Rules for ⋆
  Rule(:DualForm2, :Form0, :⋆₀, [:★, :⋆, :star]),
  Rule(:DualForm1, :Form1, :⋆₁, [:★, :⋆, :star]),
  Rule(:DualForm0, :Form2, :⋆₂, [:★, :⋆, :star]),
  Rule(:Form0, :DualForm2, :⋆₀⁻¹, [:★, :⋆, :star, :star_inv]),
  Rule(:Form1, :DualForm1, :⋆₁⁻¹, [:★, :⋆, :star, :star_inv]),
  Rule(:Form2, :DualForm0, :⋆₂⁻¹, [:★, :⋆, :star, :star_inv])
]

op2_operators = [
  # Rules for ∧.
  Rule(:Form0, [:Form0, :Form0], :∧₀₀, [:∧, :wedge]),
  Rule(:Form1, [:Form1, :Form0], :∧₁₀, [:∧, :wedge]),
  Rule(:Form1, [:Form0, :Form1], :∧₀₁, [:∧, :wedge]),
  Rule(:Form2, [:Form1, :Form1], :∧₁₁, [:∧, :wedge]),
  Rule(:Form2, [:Form2, :Form0], :∧₂₀, [:∧, :wedge]),
  Rule(:Form2, [:Form0, :Form2], :∧₀₂, [:∧, :wedge]),

  # Rules for L.
  Rule(:DualForm0, [:Form1, :DualForm0], :L₀, [:L]),
  Rule(:DualForm1, [:Form1, :DualForm1], :L₁, [:L]),
  Rule(:DualForm2, [:Form1, :DualForm2], :L₂, [:L]),

  # TODO: Make consistent with other Lie's
  Rule(:DualForm1, [:DualForm1, :DualForm1], :ℒ₁),

  # Rules for i.
  Rule(:DualForm0, [:Form1, :DualForm1], :i₁, [:i]),
  Rule(:DualForm1, [:Form1, :DualForm2], :i₂, [:i]),
  Rule(:DualForm0, [:DualForm1, :DualForm1], :ι₁₁),
  Rule(:DualForm1, [:DualForm1, :DualForm2], :ι₁₂),

  # Arthimetic rules
  arithmetic_operators(:.-, true)...,
  arithmetic_operators(:./, true)...,
  arithmetic_operators(:.*, true)...,
  arithmetic_operators(:.^, true)...,

  # TODO: Only labelled as broadcasted since Decapodes converts all these
  # to their broadcasted forms. They really should have different rules.
  arithmetic_operators(:-, true)...,
  arithmetic_operators(:/, true)...,
  arithmetic_operators(:*, true)...,
  arithmetic_operators(:^, true)...,

  # TODO: Add some intermediate result type to avoid having infers
  # Rule(:Form0, [:Form0, :infer], :^),
  # Rule(:Form0, [:Form0, :infer], :.^),
  # Rule(:Form1, [:Form1, :infer], :^),
  # Rule(:Form1, [:Form1, :infer], :.^)
]

# TODO: When SummationDecapodes are annotated with the degree of their space,
# use dispatch to choose the correct set of rules.
function default_operators(dim)
  @assert 1 <= dim <= 2
  metric_free = vcat(op1_operators, op2_operators)
  return vcat(metric_free, dim == 1 ? op1_1D_bound_operators : op1_2D_bound_operators)
end

infer_types!(d::SummationDecapode; dim = 2) =
  infer_types!(d, default_operators(dim))

resolve_overloads!(d::SummationDecapode; dim = 2) =
  resolve_overloads!(d, default_operators(dim))

type_check(d::SummationDecapode; dim = 2) =
  type_check(d, default_operators(dim))

function infer_resolve!(d::SummationDecapode; dim = 2)
  operators = default_operators(dim)
  infer_types!(d, operators)
  resolve_overloads!(d, operators)
  type_check(d, operators)

  d
end

ascii_to_unicode_op1 = Pair{Symbol, Any}[
                        (:dt       => :∂ₜ),
                        (:star     => :⋆),
                        (:lapl     => :Δ),
                        (:codif    => :δ),
                        (:avg_01    => :avg₀₁),
                        (:star_inv => :⋆⁻¹)]

ascii_to_unicode_op2 = [
                        (:wedge    => :∧)]

"""    function unicode!(d::SummationDecapode)

Replace ASCII operators with their Unicode equivalents.
"""
unicode!(d::SummationDecapode) = replace_names!(d, ascii_to_unicode_op1, ascii_to_unicode_op2)

vec_to_dec_op1 = [
                  (:grad        => :d),
                  (:div         => [:⋆,:d,:⋆]),
                  (:curl        => [:d,:⋆]),
                  (:∇           => :d),
                  (Symbol("∇ᵈ") => [:⋆,:d,:⋆]),
                  # Note: This is x, not \times.
                  (Symbol("∇x") => [:d,:⋆])]

vec_to_dec_op2 = Pair{Symbol, Symbol}[]

"""    function vec_to_dec!(d::SummationDecapode)

Replace Vector Calculus operators with Discrete Exterior Calculus equivalents.
"""
function vec_to_dec!(d::SummationDecapode)
  # Perform simple substitutions.
  replace_names!(d, vec_to_dec_op1, vec_to_dec_op2)

  # Replace `adv` with divergence of ∧.
  advs = incident(d, :adv, :op2)
  adv_tgts = d[advs, :res]

  # Intermediate wedges.
  wedge_tgts = add_parts!(d, :Var, length(adv_tgts), name=map(i -> Symbol("•_adv_$i"), eachindex(advs)), type=:infer)
  # Divergences.
  add_parts!(d, :Op1, length(adv_tgts), src=wedge_tgts, tgt=adv_tgts, op1=fill([:⋆,:d,:⋆],length(advs)))
  # Point advs to the intermediates.
  d[collect(advs), :res] = wedge_tgts

  # Replace adv with ∧.
  d[collect(advs), :op2] = fill(:∧,length(advs))

  d
end
