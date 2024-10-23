using ..DiagrammaticEquations

# TODO: You could write a method which auto-generates these rules given degree N.
"""
These are the default rules used to do type inference/function resolution in the 1D/2D exterior calculus.
"""
op1_operators = [
  # Rules for ∂ₜ
  Operator{Symbol}(:Form0, :Form0, :∂ₜ, [:dt]),
  Operator{Symbol}(:Form1, :Form1, :∂ₜ, [:dt]),
  Operator{Symbol}(:Form2, :Form2, :∂ₜ, [:dt]),

  # Rules for d.
  Operator{Symbol}(:Form1, :Form0, :d₀, [:d]),
  Operator{Symbol}(:Form2, :Form1, :d₁, [:d]),
  Operator{Symbol}(:DualForm1, :DualForm0, :dual_d₀, [:d, :d̃₀]),
  Operator{Symbol}(:DualForm2, :DualForm1, :dual_d₁, [:d, :d̃₁]),

  # Rules for δ
  Operator{Symbol}(:Form0, :Form1, :δ₁, [:δ, :codif]),
  Operator{Symbol}(:Form1, :Form2, :δ₂, [:δ, :codif]),

  # Rules for ♯
  Operator{Symbol}(:PVF, :Form1, :♯ᵖᵖ, [:♯]),
  Operator{Symbol}(:DVF, :DualForm1, :♯ᵈᵈ, [:♯]),

  # Rules for ♭
  Operator{Symbol}(:Form1, :DVF, :♭ᵈᵖ, [:♭]),

  # Rules for Δ
  Operator{Symbol}(:Form0, :Form0, :Δ₀, [:Δ, :∇², :lapl]),
  Operator{Symbol}(:Form1, :Form1, :Δ₁, [:Δ, :∇², :lapl]),
  Operator{Symbol}(:Form2, :Form2, :Δ₂, [:Δ, :∇², :lapl]),   # TODO: Test with this

  # Rules for Δᵈ
  Operator{Symbol}(:DualForm0, :DualForm0, :Δᵈ₀, [:Δ, :∇², :lapl]),
  Operator{Symbol}(:DualForm1, :DualForm1, :Δᵈ₁, [:Δ, :∇², :lapl]),

  Operator{Symbol}(:Form0, :Form0, :(-), [:neg]),
  Operator{Symbol}(:Form1, :Form1, :(-), [:neg]),
  Operator{Symbol}(:Form2, :Form2, :(-), [:neg]),
  Operator{Symbol}(:DualForm0, :DualForm0, :(-), [:neg]),
  Operator{Symbol}(:DualForm1, :DualForm1, :(-), [:neg]),
  Operator{Symbol}(:DualForm2, :DualForm2, :(-), [:neg]),

  # Rules for the averaging operator
  Operator{Symbol}(:Form1, :Form0, :avg₀₁, [:avg_01]),

  Operator{Symbol}(:Form0, :PVF, :norm, [:mag]),
  Operator{Symbol}(:DualForm0, :DVF, :norm, [:mag])
]

op1_1D_bound_operators = [

  # Rules for ⋆
  Operator{Symbol}(:DualForm1, :Form0, :⋆₀, [:★, :⋆, :star]),
  Operator{Symbol}(:DualForm0, :Form1, :⋆₁, [:★, :⋆, :star]),
  Operator{Symbol}(:Form0, :DualForm1, :⋆₀⁻¹, [:★, :⋆, :star, :star_inv]),
  Operator{Symbol}(:Form1, :DualForm0, :⋆₁⁻¹, [:★, :⋆, :star, :star_inv])
]

op1_2D_bound_operators = [

  # Rules for ⋆
  Operator{Symbol}(:DualForm2, :Form0, :⋆₀, [:★, :⋆, :star]),
  Operator{Symbol}(:DualForm1, :Form1, :⋆₁, [:★, :⋆, :star]),
  Operator{Symbol}(:DualForm0, :Form2, :⋆₂, [:★, :⋆, :star]),
  Operator{Symbol}(:Form0, :DualForm2, :⋆₀⁻¹, [:★, :⋆, :star, :star_inv]),
  Operator{Symbol}(:Form1, :DualForm1, :⋆₁⁻¹, [:★, :⋆, :star, :star_inv]),
  Operator{Symbol}(:Form2, :DualForm0, :⋆₂⁻¹, [:★, :⋆, :star, :star_inv])
]

op2_operators = [
  # Rules for ∧.
  Operator{Symbol}(:Form0, [:Form0, :Form0], :∧₀₀, [:∧, :wedge]),
  Operator{Symbol}(:Form1, [:Form1, :Form0], :∧₁₀, [:∧, :wedge]),
  Operator{Symbol}(:Form1, [:Form0, :Form1], :∧₀₁, [:∧, :wedge]),
  Operator{Symbol}(:Form2, [:Form1, :Form1], :∧₁₁, [:∧, :wedge]),
  Operator{Symbol}(:Form2, [:Form2, :Form0], :∧₂₀, [:∧, :wedge]),
  Operator{Symbol}(:Form2, [:Form0, :Form2], :∧₀₂, [:∧, :wedge]),

  # Rules for L.
  Operator{Symbol}(:DualForm0, [:Form1, :DualForm0], :L₀, [:L]),
  Operator{Symbol}(:DualForm1, [:Form1, :DualForm1], :L₁, [:L]),
  Operator{Symbol}(:DualForm2, [:Form1, :DualForm2], :L₂, [:L]),

  # TODO: Make consistent with other Lie's
  Operator{Symbol}(:DualForm1, [:DualForm1, :DualForm1], :ℒ₁),

  # Rules for i.
  Operator{Symbol}(:DualForm0, [:Form1, :DualForm1], :i₁, [:i]),
  Operator{Symbol}(:DualForm1, [:Form1, :DualForm2], :i₂, [:i]),
  Operator{Symbol}(:DualForm0, [:DualForm1, :DualForm1], :ι₁₁),
  Operator{Symbol}(:DualForm1, [:DualForm1, :DualForm2], :ι₁₂),

  # Arthimetic rules
  arthimetic_operators(:.-, true)...,
  arthimetic_operators(:./, true)...,
  arthimetic_operators(:.*, true)...,
  arthimetic_operators(:.^, true)...,

  # TODO: Only labelled as broadcasted since Decapodes converts all these
  # to their broadcasted forms. They really should have differnt rules.
  arthimetic_operators(:-, true)...,
  arthimetic_operators(:/, true)...,
  arthimetic_operators(:*, true)...,
  arthimetic_operators(:^, true)...,

  # TODO: Add some intermediate result type to avoid having infers
  # Operator{Symbol}(:Form0, [:Form0, :infer], :^),
  # Operator{Symbol}(:Form0, [:Form0, :infer], :.^),
  # Operator{Symbol}(:Form1, [:Form1, :infer], :^),
  # Operator{Symbol}(:Form1, [:Form1, :infer], :.^)
]

# TODO: When SummationDecapodes are annotated with the degree of their space,
function default_operators(dim)
  @assert 1 <= dim <= 2
  metric_free = vcat(op1_operators, op2_operators)
  return vcat(metric_free, dim == 1 ? op1_1D_bound_operators : op1_2D_bound_operators)
end

infer_types!(d::SummationDecapode; dim = 2) =
  infer_types!(d, default_operators(dim))

resolve_overloads!(d::SummationDecapode; dim = 2) =
  resolve_overloads!(d, default_operators(dim))

function infer_resolve!(d::SummationDecapode; dim = 2)
  operators = default_operators(dim)
  infer_types!(d, operators)
  resolve_overloads!(d, operators)

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

# TODO: When SummationDecapodes are annotated with the degree of their space,
# use dispatch to choose the correct set of rules.
