using Test
using DiagrammaticEquations

@testset "Canon Inference and Overloading" begin
  function check_canontyping(control::SummationDecapode, test::SummationDecapode)
    infer_test = infer_types!(deepcopy(test))
    infer_control = infer_types!(deepcopy(control))
    @test infer_control[:type] == infer_test[:type]
    @test infer_test[:op1] == test[:op1]
    @test infer_test[:op2] == test[:op2]
  end

  function check_canonoverload_op1(control::SummationDecapode, test::SummationDecapode)
    over_test = resolve_overloads!(infer_types!(deepcopy(test)))
    over_control = resolve_overloads!(infer_types!(deepcopy(control)))
    @test get_canon_name(over_test[:op1]) == get_canon_name(over_control[:op1])
  end

  setup_basecase(d::SummationDecapode) = resolve_overloads!(infer_types!(deepcopy(d)))

  # Test exterior derivative and hodge

  gen_d = @decapode begin
    A::Form0
    B == d(hdg(hdg(A)))
    C == d(hdg(hdg(B)))
    D == hdg(hdg(C))
    E == d(d(hdg(D)))
  end

  let # Check base case explicitly
    d = setup_basecase(gen_d)
    @test d[:type]  == [:Form0, :Form1, :DualForm2, :Form0, :DualForm2, :Form2, :DualForm0, :Form1, :DualForm1, :Form2, :DualForm1, :DualForm0]
    @test get_canon_name(d[:op1]) == [:hdg_0, :invhdg_0, :d_0, :hdg_1, :invhdg_1, :d_1, :hdg_2, :invhdg_2, :hdg_2, :dual_d_0, :dual_d_1]
  end

  let # Ascii and tagged
    d = @decapode begin
      A::Form0
      B == d_0(invhdg_0(hdg_0(A)))
      C == d_1(invhdg_1(hdg_1(B)))
      D == invhdg_2(hdg_2(C))
      E == dual_d_1(dual_d_0(hdg_2(D)))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  let # Unicode and tagged
    d = @decapode begin
      A::Form0
      B == d₀(⋆₀⁻¹(⋆₀(A)))
      C == d₁(⋆₁⁻¹(⋆₁(B)))
      D == ⋆₂⁻¹(⋆₂(C))
      E == d̃₁(d̃₀(⋆₂(D)))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  let # Unicode and not tagged
    d = @decapode begin
      A::Form0
      B == d(⋆(⋆(A)))
      C == d(⋆(⋆(B)))
      D == ⋆(⋆(C))
      E == d̃(d̃(⋆(D)))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  let # Combination of names
    d = @decapode begin
      A::Form0
      B == d(⋆(hdg(A)))
      C == d₁(hdg(⋆₁(B)))
      D == invhdg_2(⋆₂(C))
      E == d̃₁(dual_d_0(hdg_2(D)))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  # Test laplacian and codifferential

  gen_d = @decapode begin
    A::Form0
    B == codif(codif(lapl(d(lapl(d(lapl(A)))))))
  end

  let # Check base case explicitly
    d = setup_basecase(gen_d)
    @test d[:type]  == [:Form0, :Form0, :Form0, :Form1, :Form2, :Form2, :Form1, :Form1]
    @test get_canon_name(d[:op1]) == [:lapl_0, :d_0, :lapl_1, :d_1, :lapl_2, :codif_2, :codif_1]
  end

  let # Ascii and tagged
    d = @decapode begin
      A::Form0
      B == codif_1(codif_2(lapl_2(d_1(lapl_1(d_0(lapl_0(A)))))))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  let # Unicode and tagged
    d = @decapode begin
      A::Form0
      B == δ₁(δ₂(Δ₂(d₁(Δ₁(d₀(Δ₀(A)))))))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  let # Unicode and not tagged
    d = @decapode begin
      A::Form0
      B == δ(δ(Δ(d(Δ(d(Δ(A)))))))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  let # Combination of names
    d = @decapode begin
      A::Form0
      B == δ(codif(Δ₂(d₁(lapl(d_0(Δ(A)))))))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  # Test average, neg and mag

  gen_d = @decapode begin
    A::Form0
    C::Form2
    B == neg(mag(avg(neg(mag(A)))))
    D == neg(mag(C))
  end

  let # Check base case explicitly
    d = setup_basecase(gen_d)
    @test d[:type]  == [:Form0, :Form2, :Form1, :Form2, :Form1, :Form1, :Form0, :Form0, :Form2]
    @test get_canon_name(d[:op1]) == [:mag, :-, :avg_01, :mag, :-, :mag, :-]
  end
  let # Alternate names
    d = @decapode begin
      A::Form0
      C::Form2
      B == -(norm(avg_01(-(norm(A)))))
      D == -(norm(C))
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op1(gen_d, d)
  end

  function check_canonoverload_op2(control::SummationDecapode, test::SummationDecapode)
    over_test = resolve_overloads!(infer_types!(deepcopy(test)))
    over_control = resolve_overloads!(infer_types!(deepcopy(control)))
    @test get_canon_name(over_test[:op2]) == get_canon_name(over_control[:op2])
  end

  # Wedge products
  gen_d = @decapode begin
    (A, B)::Form0
    (C, D)::Form1
    E::Form2

    R00 == wdg(A, B)
    R01 == wdg(A, C)
    R10 == wdg(C, A)

    R11 == wdg(C, D)
    R02 == wdg(A, E)
    R20 == wdg(E, A)
  end

  let
    d = setup_basecase(gen_d)
    @test d[:type] == [:Form0, :Form0, :Form1, :Form1, :Form2, :Form0, :Form2, :Form1, :Form2, :Form1, :Form2]
    @test get_canon_name(d[:op2]) == [:wdg_00, :wdg_01, :wdg_10, :wdg_11, :wdg_02, :wdg_20]
  end

  let #Unicode, tagged
    d = @decapode begin
      (A, B)::Form0
      (C, D)::Form1
      E::Form2

      R00 == ∧₀₀(A, B)
      R01 == ∧₀₁(A, C)
      R10 == ∧₁₀(C, A)

      R11 == ∧₁₁(C, D)
      R02 == ∧₀₂(A, E)
      R20 == ∧₂₀(E, A)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  let # Unicode and not tagged
    d = @decapode begin
      (A, B)::Form0
      (C, D)::Form1
      E::Form2

      R00 == ∧(A, B)
      R01 == ∧(A, C)
      R10 == ∧(C, A)

      R11 == ∧(C, D)
      R02 == ∧(A, E)
      R20 == ∧(E, A)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  let # Ascii tagged
    d = @decapode begin
      (A, B)::Form0
      (C, D)::Form1
      E::Form2

      R00 == wdg_00(A, B)
      R01 == wdg_01(A, C)
      R10 == wdg_10(C, A)

      R11 == wdg_11(C, D)
      R02 == wdg_02(A, E)
      R20 == wdg_20(E, A)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  let # Mixed
    d = @decapode begin
      (A, B)::Form0
      (C, D)::Form1
      E::Form2

      R00 == ∧(A, B)
      R01 == ∧₀₁(A, C)
      R10 == wdg_10(C, A)

      R11 == ∧₁₁(C, D)
      R02 == ∧(A, E)
      R20 == wdg_20(E, A)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  # Wedge products
  gen_d = @decapode begin
    A::DualForm0
    B::DualForm1
    C::DualForm2
    D::Form1

    R0 == L(D, A)
    R1 == L(D, B)
    R2 == L(D, C)

    R3 == i(D, B)
    R4 == i(D, C)
  end

  let
    d = setup_basecase(gen_d)
    @test d[:type] == [:DualForm0, :DualForm1, :DualForm2, :Form1, :DualForm0, :DualForm1, :DualForm1, :DualForm0, :DualForm2]
    @test d[:op2] == [:L₀, :L₁, :L₂, :i₁, :i₂]
  end

  let # Unicode special, tagged
    d = @decapode begin
      A::DualForm0
      B::DualForm1
      C::DualForm2
      D::Form1

      R0 == ℒ₀(D, A)
      R1 == ℒ₁(D, B)
      R2 == ℒ₂(D, C)

      R3 == ι₁(D, B)
      R4 == ι₂(D, C)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  let # Unicode, not tagged
    d = @decapode begin
      A::DualForm0
      B::DualForm1
      C::DualForm2
      D::Form1

      R0 == ℒ(D, A)
      R1 == ℒ(D, B)
      R2 == ℒ(D, C)

      R3 == ι(D, B)
      R4 == ι(D, C)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  let # Unicode, tagged
    d = @decapode begin
      A::DualForm0
      B::DualForm1
      C::DualForm2
      D::Form1

      R0 == L₀(D, A)
      R1 == L₁(D, B)
      R2 == L₂(D, C)

      R3 == i₁(D, B)
      R4 == i₂(D, C)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  let # Ascii, tagged
    d = @decapode begin
      A::DualForm0
      B::DualForm1
      C::DualForm2
      D::Form1

      R0 == L_0(D, A)
      R1 == L_1(D, B)
      R2 == L_2(D, C)

      R3 == i_1(D, B)
      R4 == i_2(D, C)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end

  let # Mixed
    d = @decapode begin
      A::DualForm0
      B::DualForm1
      C::DualForm2
      D::Form1

      R0 == ℒ(D, A)
      R1 == L_1(D, B)
      R2 == L₂(D, C)

      R3 == i_1(D, B)
      R4 == i₂(D, C)
    end
    check_canontyping(gen_d, d)
    check_canonoverload_op2(gen_d, d)
  end
end

@testset "Typed Function Observance" begin
  let # Typing respects typed exterior derivative
    d = @decapode begin
      A::Form2
      B0 == d_0(A)
      B2 == d_1(A)
    end
    infer_types!(d)
    @test d[:type] == [:Form2, :infer, :infer]
  end

  let # Typing respects typed dual derivatives
    d = @decapode begin
      A::DualForm2
      B0 == dual_d_0(A)
      B2 == duald_1(A)
    end
    infer_types!(d)
    @test d[:type] == [:DualForm2, :infer, :infer]
  end

  let # Typing respects typed hodges
    d = @decapode begin
      A::Form0
      A2::Form2
      B0 == hdg_0(A2)
      B1 == hdg_1(A)
      B2 == hdg_2(A)
    end
    infer_types!(d)
    @test d[:type] == [:Form0, :Form2, :infer, :infer, :infer]
  end

  let # Typing respects typed inverse hodges
    d = @decapode begin
      A::DualForm2
      A2::DualForm0
      B0 == invhdg_0(A2)
      B1 == invhdg_1(A)
      B2 == invhdg_2(A)
    end
    infer_types!(d)
    @test d[:type] == [:DualForm2, :DualForm0, :infer, :infer, :infer]
  end

  let # Typing respects typed laplacians
    d = @decapode begin
      A::Form0
      A2::Form2
      B0 == lapl_0(A2)
      B1 == lapl_1(A)
      B2 == lapl_2(A)
    end
    infer_types!(d)
    @test d[:type] == [:Form0, :Form2, :infer, :infer, :infer]
  end

  let # Typing respects typed codifferentials
    d = @decapode begin
     A::Form0
     B0 == codif_1(A)
     B1 == codif_2(A)
    end
    infer_types!(d)
    @test d[:type] == [:Form0, :infer, :infer]
  end

  let # Typing respects typed average
    d = @decapode begin
      A::Form2
      B == avg_01(A)
    end
    infer_types!(d)
    @test d[:type] == [:Form2, :infer]
  end

  let # Typing respects typed wedge
    d = @decapode begin
      (A, B)::Form0
      C::Form1
      R00 == wdg_00(A, C)
      R01 == wdg_01(A, B)
      R10 == wdg_10(A, B)

      R11 == wdg_11(A, B)
      R02 == wdg_02(A, B)
      R20 == wdg_20(A, B)
    end
    infer_types!(d)
    @test d[:type] == [:Form0, :Form0, :Form1, :infer, :infer, :infer, :infer, :infer, :infer]

  end

  let # Typing respects typed lie and inner
    d = @decapode begin
      D::Form1

      R0 == L_0(D, D)
      R1 == L_1(D, D)
      R2 == L_2(D, D)

      R3 == i_1(D, D)
      R4 == i_2(D, D)
    end
    infer_types!(d)
    @test d[:type] == [:Form1, :infer, :infer, :infer, :infer, :infer]
  end

end

@testset "Overloading Canon Name Check" begin

  setup_decapode!(d::SummationDecapode) = resolve_overloads!(infer_types!(d))

  let # Check ascii derivative and hodge
    d = @decapode begin
      A::Form0
      B == d(hdg(hdg(A)))
      C == d(hdg(hdg(B)))
      D == hdg(hdg(C))
      E == d(d(hdg(D)))
    end
    setup_decapode!(d)
    @test d[:op1] == [:hdg_0, :invhdg_0, :d₀, :hdg_1, :invhdg_1, :d₁, :hdg_2, :invhdg_2, :hdg_2, :dual_d₀, :dual_d₁]
  end

  let # Check unicode derivative and hodge
    d = @decapode begin
      A::Form0
      B == d(⋆(⋆(A)))
      C == d(⋆(⋆(B)))
      D == ⋆(⋆(C))
      E == d̃(d̃(⋆(D)))
    end
    setup_decapode!(d)
    @test d[:op1] == [:⋆₀, :⋆₀⁻¹, :d₀, :⋆₁, :⋆₁⁻¹, :d₁, :⋆₂, :⋆₂⁻¹, :⋆₂, :d̃₀, :d̃₁]
  end

  let # Check ascii codif and lapl
    d = @decapode begin
      A::Form0
      B == codif(codif(lapl(d(lapl(d(lapl(A)))))))
    end

    setup_decapode!(d)
    @test d[:op1] == [:lapl_0, :d₀, :lapl_1, :d₁, :lapl_2, :codif_2, :codif_1]
  end

  let # Check unicode codif and lapl
    d = @decapode begin
      A::Form0
      B == δ(δ(Δ(d(Δ(d(Δ(A)))))))
    end

    setup_decapode!(d)
    @test d[:op1] == [:Δ₀, :d₀, :Δ₁, :d₁, :Δ₂, :δ₂, :δ₁]
  end

  let # Check ascii wedge
    d = @decapode begin
      (A, B)::Form0
      (C, D)::Form1
      E::Form2

      R00 == wdg(A, B)
      R01 == wdg(A, C)
      R10 == wdg(C, A)

      R11 == wdg(C, D)
      R02 == wdg(A, E)
      R20 == wdg(E, A)
    end
    setup_decapode!(d)
    @test d[:op2] == [:wdg_00, :wdg_01, :wdg_10, :wdg_11, :wdg_02, :wdg_20]
  end

  let # Check unicode wedge
    d = @decapode begin
      (A, B)::Form0
      (C, D)::Form1
      E::Form2

      R00 == ∧(A, B)
      R01 == ∧(A, C)
      R10 == ∧(C, A)

      R11 == ∧(C, D)
      R02 == ∧(A, E)
      R20 == ∧(E, A)
    end
    setup_decapode!(d)
    @test d[:op2] == [:∧₀₀, :∧₀₁, :∧₁₀, :∧₁₁, :∧₀₂, :∧₂₀]
  end

  let # Check ascii lie and inner
    d = @decapode begin
      A::DualForm0
      B::DualForm1
      C::DualForm2
      D::Form1

      R0 == L(D, A)
      R1 == L(D, B)
      R2 == L(D, C)

      R3 == i(D, B)
      R4 == i(D, C)
    end
    setup_decapode!(d)
    @test d[:op2] == [:L₀, :L₁, :L₂, :i₁, :i₂]
  end

  let # Check unicode lie and inner
    d = @decapode begin
      A::DualForm0
      B::DualForm1
      C::DualForm2
      D::Form1

      R0 == ℒ(D, A)
      R1 == ℒ(D, B)
      R2 == ℒ(D, C)

      R3 == ι(D, B)
      R4 == ι(D, C)
    end
    setup_decapode!(d)
    @test d[:op2] == [:ℒ₀, :ℒ₁, :ℒ₂, :ι₁, :ι₂]
  end

end

# Some of these tests were originally in test/language.jl.
@testset "ASCII Operators" begin
# Test ASCII to Unicode conversion on an Op2.
t1 = @decapode begin
  A == wedge(C, D)
end
unicode!(t1)

op2s_1 = Set(t1[:op2])
op2s_expected_1 = Set([:∧])
@test issetequal(op2s_1, op2s_expected_1)

# Test ASCII to Unicode conversion with multiple occurences of the same operator.
t2 = @decapode begin
  A == wedge(C, D)
  B == wedge(E, F)
end
unicode!(t2)

op2s_2 = Set(t2[:op2])
op2s_expected_2 = Set([:∧])
@test issetequal(op2s_2, op2s_expected_2)

# Test ASCII to Unicode conversion works with composed operators after expansion.
t3 = @decapode begin
  A == ∘(star, lapl, star)(B)
end
t3 = expand_operators(t3)
unicode!(t3)

op1s_3 = Set(t3[:op1])
op1s_expected_3 = Set([:⋆,:Δ])
@test issetequal(op1s_3, op1s_expected_3)

# Test ASCII tangent operator identifies a TVar.
t4 = @decapode begin
  A == dt(B)
end
@test nparts(t4, :TVar) == 1

# Test ASCII to Unicode conversion on all transformable operators.
t5 = @decapode begin
  dt(A) == ∘(d, d_0, d_1, dual_d₀, dual_d₁,
             hdg, hdg_0, hdg_1, hdg_2,
             star, star_0, star_1, star_2,
             invhdg_0, invhdg_1, invhdg_2,
             inv_star_0, inv_star_1, inv_star_2,
             lapl, lapl_0, lapl_1, lapl_2,
             codif, codif_1, codif_2,
             neg, norm, avg_01)(B)
  C == wdg(D,E) + wdg_00(D,E) + wdg_01(D,E) + wdg_10(D,E) + wdg_11(D,E) + wdg_02(D,E) + wdg_20(D,E)
  F == L(H,G) + L_0(H,G) + L_1(H,G) + L_2(H,G) + L₀(H,G) + L₁(H,G) + L₂(H,G)
  G == i(H,I) + i_1(F,I) + i_2(F,I) + i₁(F,I) + i₂(F,I)
end
t5 = unicode!(expand_operators(t5))
@test issetequal(t5[:op1], [:∂ₜ, :d, :d₀, :d₁, :d̃₀, :d̃₁, :⋆, :⋆₀, :⋆₁, :⋆₂,
                            :⋆₀⁻¹, :⋆₁⁻¹, :⋆₂⁻¹, :Δ, :Δ₀, :Δ₁, :Δ₂, :δ, :δ₁, :δ₂,
                            :avg₀₁, :-, :mag])
@test issetequal(t5[:op2], [:∧, :∧₀₀, :∧₀₁, :∧₁₀, :∧₁₁, :∧₀₂, :∧₂₀,
                            :ℒ, :ℒ₀, :ℒ₁, :ℒ₂, :ι, :ι₁, :ι₂])
end # testset
