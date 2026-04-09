using Test
using DiagrammaticEquations
using Catlab.ACSetInterface

# Upset (ancestor subgraph)
# #########################

# A root variable with no ancestors returns just that variable.
let
  d = @decapode begin
    C::Form0
  end
  e = upset(d, :C)
  @test nparts(e, :Var) == 1
  @test e[1, :name] == :C
  @test nparts(e, :Op1) == 0
  @test nparts(e, :Op2) == 0
  @test nparts(e, :Σ) == 0
end

# Upset of a variable produced by a single Op1 chain.
let
  d = @decapode begin
    B == f(A)
    C == g(B)
  end
  e = upset(d, :C)
  @test nparts(e, :Var) == 3
  @test Set(e[:name]) == Set([:A, :B, :C])
  @test nparts(e, :Op1) == 2
  @test Set(e[:op1]) == Set([:f, :g])
end

# Upset of an intermediate variable includes only its ancestors.
let
  d = @decapode begin
    B == f(A)
    C == g(B)
  end
  e = upset(d, :B)
  @test nparts(e, :Var) == 2
  @test Set(e[:name]) == Set([:A, :B])
  @test nparts(e, :Op1) == 1
  @test e[1, :op1] == :f
end

# Upset through an Op2 includes both projections.
let
  d = @decapode begin
    C == A * B
  end
  e = upset(d, :C)
  @test nparts(e, :Var) == 3
  @test Set(e[:name]) == Set([:A, :B, :C])
  @test nparts(e, :Op2) == 1
  @test e[1, :op2] == :*
end

# Upset through a summation includes all summands.
let
  d = @decapode begin
    D == A + B + C
  end
  e = upset(d, :D)
  @test nparts(e, :Var) == 4
  @test Set(e[:name]) == Set([:A, :B, :C, :D])
  @test nparts(e, :Σ) == 1
end

# Upset excludes unrelated branches.
let
  d = @decapode begin
    B == f(A)
    D == g(C)
  end
  e = upset(d, :B)
  @test nparts(e, :Var) == 2
  @test Set(e[:name]) == Set([:A, :B])
  @test nparts(e, :Op1) == 1
end

# Upset with shared ancestors includes them once.
let
  d = @decapode begin
    B == f(A)
    C == g(A)
    D == B * C
  end
  e = upset(d, :D)
  @test nparts(e, :Var) == 4
  @test Set(e[:name]) == Set([:A, :B, :C, :D])
  @test nparts(e, :Op1) == 2
  @test nparts(e, :Op2) == 1
end

# Combined Op1, Op2, and Σ in the upset.
let
  d = @decapode begin
    F == h(d(A) + f(g(B) * C) + D)
  end
  e = upset(d, :F)
  # F depends on everything.
  @test nparts(e, :Var) == nparts(d, :Var)
  @test nparts(e, :Op1) == nparts(d, :Op1)
  @test nparts(e, :Op2) == nparts(d, :Op2)
  @test nparts(e, :Σ) == nparts(d, :Σ)
end

# Error on nonexistent variable.
let
  d = @decapode begin
    B == f(A)
  end
  @test_throws Exception upset(d, :Z)
end
