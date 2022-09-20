using Catlab
using CombinatorialSpaces
using CombinatorialSpaces.ExteriorCalculus
using Decapodes
using MultiScaleArrays
using OrdinaryDiffEq
using MLStyle
using Distributions
using LinearAlgebra

function generate(sd, my_symbol)
  op = @match my_symbol begin
    :k => x->x/20
    :⋆₀ => x->⋆(0,sd,hodge=DiagonalHodge())*x
    :⋆₁ => x->⋆(1, sd, hodge=DiagonalHodge())*x
    :⋆₀⁻¹ => x->inv_hodge_star(0,sd, x; hodge=DiagonalHodge())
    :⋆₁⁻¹ => x->inv_hodge_star(1,sd,hodge=DiagonalHodge())*x
    :d₀ => x->d(0,sd)*x
    :dual_d₀ => x->dual_derivative(0,sd)*x
    :dual_d₁ => x->dual_derivative(1,sd)*x
    :∧₀₁ => (x,y)-> wedge_product(Tuple{0,1}, sd, x, y)
    :plus => (+)
  end
  # return (args...) -> begin println("applying $my_symbol"); println("arg length $(length(args[1]))"); op(args...);end
  return (args...) ->  op(args...)
end


DiffusionExprBody =  quote
    C::Form0{X}
    Ċ::Form0{X}
    ϕ::Form1{X}

    # Fick's first law
    ϕ ==  ∘(d₀, k)(C)
    # Diffusion equation
    Ċ == ∘(⋆₁, dual_d₁, ⋆₀⁻¹)(ϕ)
    ∂ₜ(C) == Ċ
end


diffExpr = parse_decapode(DiffusionExprBody)
ddp = NamedDecapode(diffExpr)
gensim(expand_operators(ddp), [:C])
f = eval(gensim(expand_operators(ddp), [:C]))

include("spherical_meshes.jl")
radius = 6371+90
primal_earth, npi, spi = makeSphere(0, 180, 5, 0, 360, 5, radius);
nploc = primal_earth[npi, :point]
orient!(primal_earth)
earth = EmbeddedDeltaDualComplex2D{Bool,Float64,Point3D}(primal_earth)
subdivide_duals!(earth, Circumcenter())


fₘ = f(earth)
c_dist = MvNormal(nploc[[1,2]], 100[1, 1])
c = [pdf(c_dist, [p[1], p[2]]./√radius) for p in earth[:point]]

u₀ = construct(PhysicsState, [VectorForm(c)],Float64[], [:C])
tₑ = 10
prob = ODEProblem(fₘ,u₀,(0,tₑ))
soln = solve(prob, Tsit5())

using GLMakie

mesh(primal_earth, color=findnode(soln(0), :C), colormap=:plasma)
mesh(primal_earth, color=findnode(soln(tₑ), :C), colormap=:plasma)
mesh(primal_earth, color=findnode(soln(tₑ)-soln(0), :C), colormap=:plasma)

AdvDiff = quote
    C::Form0{X}
    Ċ::Form0{X}
    V::Form1{X}
    ϕ::Form1{X}
    ϕ₁::Form1{X}
    ϕ₂::Form1{X}

    # Fick's first law
    ϕ₁ ==  (d₀∘k)(C)
    ϕ₂ == ∧₀₁(C,V)
    ϕ == ϕ₁ + ϕ₂
    # Diffusion equation
    Ċ == ∘(⋆₁, dual_d₁,⋆₀⁻¹)(ϕ)
    ∂ₜ(C) == Ċ
end

advdiff = parse_decapode(AdvDiff)
advdiffdp = NamedDecapode(advdiff)
gensim(expand_operators(advdiffdp), [:C, :V])
sim = eval(gensim(expand_operators(advdiffdp), [:C, :V]))

fₘ = sim(earth)

# velocity(p) = [-p[2]/p[1], 1.0, 0]/log(abs(p[3])+1)
velocity(p) = [-p[2]/p[1], 1.0, sign(p[1]*abs(p[3]))]#/log(abs(p[3])+1)
velocity(p) = [0, 0, sign(p[1]*abs(p[3]))]#/log(abs(p[3])+1)
v = flat_op(earth, DualVectorField(velocity.(earth[triangle_center(earth),:dual_point])); dims=[30, 10, Inf])
c_dist = MvNormal(nploc[[1,2]], 100*[1, 1])
c = [pdf(c_dist, [p[1], p[2]]) for p in earth[:point]]

u₀ = construct(PhysicsState, [VectorForm(c), VectorForm(1000v)],Float64[], [:C, :V])
tₑ = 20
prob = ODEProblem(fₘ,u₀,(0,tₑ))
soln = solve(prob, Tsit5())

mesh(primal_earth, color=findnode(soln(tₑ), :C), colormap=:plasma)


cords(mesh) = begin
  dim(mesh, i) = [p[i] for p in unique(mesh[:point]) if p[3] > 0]
  return dim.([mesh], [1,2,3])
end
# surface(cords(earth)..., axis=(type=Axis3,))
# scene = Scene();
# arr = Makie.arrows!(
#     cords(earth)..., ones(nv(earth)), ones(nv(earth)), ones(nv(earth));
#     arrowsize = 10.1, linecolor = (:gray, 0.7), linewidth = 0.02, lengthscale = 0.1
# )

velocity(p) = [-p[2]/p[1], 1.0, sign(p[1]*abs(p[3]))]#/log(abs(p[3])+1)
ps = earth[:point]
ns = 100*Vec3f.(map(velocity, ps))
arrows(
    ps, ns, fxaa=true, # turn on anti-aliasing
    linecolor = :gray, arrowcolor = :gray,
    linewidth = 20.1, arrowsize = 20*Vec3f(3, 3, 4),
    align = :center, axis=(type=Axis3,)
)