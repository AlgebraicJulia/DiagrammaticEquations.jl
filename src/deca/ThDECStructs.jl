using SymbolicUtils
import SymbolicUtils.Symbolic
using SymbolicUtils.Rewriters

abstract type Form <: Number end
struct ZeroForm <: Form end

struct d
  dim::Int
end

(op::d)(x::Symbolic) = SymbolicUtils.Term{Form}(op, Any[x])

# Base.nameof(op::d) = Symbol("d$('₀'+op.dim)")

@syms x::Form
@syms y::Form

f = d(0)(x)


d₀rule = @rule d(1)(d(0)(~x)) => ZeroForm()
drule = @rule d(~i+1)(d(~i)(~x)) => ZeroForm()

using Test
@test_broken d₀rule(d(1)(d(0)(x))) == ZeroForm()
d₀rule(d(1)(d(0)(x))) == ZeroForm()
@test d₀rule(d(2)(d(1)(x))) == nothing # accidentally passing
@test_broken drule(d(1)(d(0)(x))) == ZeroForm()
@test_broken drule(d(2)(d(1)(x))) == ZeroForm()
@test drule(d(3)(d(1)(x))) == nothing # accidentally passing