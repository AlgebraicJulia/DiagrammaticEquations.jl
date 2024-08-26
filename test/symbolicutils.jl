using SymbolicUtils
import SymbolicUtils: promote_symtype, symtype
using DiagrammaticEquations
using DiagrammaticEquations.ThDEC
using DiagrammaticEquations.SymbolicUtilsInterop
import DiagrammaticEquations.SymbolicUtilsInterop: DECType
using MLStyle

# Ω₀ = Form(0, false, Space(:X,1))
Ω₀ = FormT{0,false, :X, 1}
Ω₁ = FormT{1,false, :X, 1}
# @syms x f(x::Sort)::Sort
@syms x f(x::DECType)::DECType
@syms y::Ω₀ v::Ω₁

fdim(::FormT{i,isdual,space,dim}) where {i,isdual,space,dim} = i
isdual(::FormT{i,duality,space,dim}) where {i,duality,space,dim} = duality
spacename(::FormT{i,isdual,X,dim}) where {i,isdual,X,dim} = X
sdim(::FormT{i,isdual,space,dim}) where {i,isdual,space,dim} = dim

fdim(::PrimalFormT{i,space,dim}) where {i,space,dim} = i
isdual(::PrimalFormT{i,space,dim}) where {i,space,dim} = false
spacename(::PrimalFormT{i,X,dim}) where {i,X,dim} = X
sdim(::PrimalFormT{i,space,dim}) where {i,space,dim} = dim

function SymbolicUtils.promote_symtype(f::typeof(f), ::T) where T<: DECType
  @show "Hit me"
  FormT{fdim(T)+1, !isdual(T), spacename(T), sdim(T)}
end

# function SymbolicUtils.promote_symtype(f::typeof(f), ::FormT{i,isdual,space,dim}) where {i,isdual,space,dim}
#   @show "Hit me"
#   FormT{i+1, !isdual, space, dim}
# end

# function SymbolicUtils.promote_symtype(f::typeof(f), args...) 
#   @show args
#   @match args[1] begin
#     Scalar() => Scalar()
#     FormT(&i, &isdual, &space, &dim) => FormT(i+1, isdual, space, dim)
#     _ => error("type checking error for f")
#   end
# end

using Test
@test_throws Exception f(x)
@test_throws Exception f(x)
# @less f(y)
symtype(f(y))
promote_symtype(f, symtype(y))
@syms x f(x::DECType)
promote_symtype(f, symtype(y))