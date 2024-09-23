using Test
using DiagrammaticEquations
using SymbolicUtils
using SymbolicUtils: Fixpoint, Prewalk, Postwalk, Chain, symtype, promote_symtype
using MLStyle


Brusselator = @decapode begin
  (U, V)::Form0
  U2V::Form0
  (U̇, V̇)::Form0

  (α)::Constant
  F::Parameter

  U2V == (U .* U) .* V

  U̇ == 1 + U2V - (4.4 * U) + (α * Δ(U)) + F
  V̇ == (3.4 * U) - U2V + (α * Δ(V))
  ∂ₜ(U) == U̇
  ∂ₜ(V) == V̇
end
infer_types!(Brusselator)


# it seems that type-instability or improper type promotion is happening. expressions derived from this have BasicSymbolic{Number} type, which means we can't conditionally rewrite on forms.
heat_exprs = symbolic_rewriting(Heat)
sub = heat_exprs[1].arguments[2].arguments[2]

a, b = @syms a::Scalar b::Scalar
u, v = @syms u::PrimalForm{0, :X, 2} du::PrimalForm{1, :X, 2}

r = rules(Δ, Val(1))

# rule without predication works
R = @rule Δ(~x) => ★(d(★(d(~x))))
rwR = Fixpoint(Prewalk(Chain([R])))

R(Δ(d(u)))

# since promote_symtype(d(u)) returns Any while promote_symtype(d, u). I wonder
# if `d(u)` is not subjected to `symtype`

Rp = @rule Δ(~x::isForm1) => "Success"
Rp(Δ(v)) # works
Rp(Δ(d(u))) # works

Rp1 = @rule Δ(~x::isForm1) => ★(d(★(d(~x))))

Rp1(Δ(v)) # works
Rp1(Δ(d(u))) # works
rwRp1 = Fixpoint(Prewalk(Chain([Rp1])))
rwRp1(Δ(d(u)))

rwr = Fixpoint(Prewalk(Chain(r)))
rwr(heat_exprs[1]) # THIS WORKS!

rwr(Δ(d(u))) # rwr
rwr(heat_exprs[1].arguments[2])

r[2](Δ(d(u))) # works


# rwR(heat_exprs[1])
# rwR(sub)

# tilde?
R1 = @rule Δ(~~x::(x->isForm1(x))) => ★(d(★(d(~x))))

@macroexpand @rule Δ(~x::isForm1) => "Success"

# pulling out the subexpression
rewriter = SymbolicUtils.Fixpoint(SymbolicUtils.Prewalk(SymbolicUtils.Chain(r)))

res_exprs =  apply_rewrites(heat_exprs, rewriter)
sub_exprs =  apply_rewrites([sub], rewriter)

optm_dd_0 = @rule d(d(~x)) => 0
star_0 = @rule ★(0) => 0
d_0 = @rule d(0) => 0

optm_rewriter = SymbolicUtils.Postwalk(
  SymbolicUtils.Fixpoint(SymbolicUtils.Chain([optm_dd_0, star_0, d_0])))

res_merge_exprs = map(optm_rewriter, res_exprs)

deca_test = to_acset(Heat, res_exprs)
infer_types!(deca_test)
resolve_overloads!(deca_test)
