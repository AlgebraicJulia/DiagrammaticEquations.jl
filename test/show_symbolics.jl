using DiagrammaticEquations

# TODO: migrate this to src/symbolictheoryutils.jl
using SymbolicUtils
import SymbolicUtils: symtype, issym
import Base: nameof
import SymbolicUtils.show_call

nameof(f, arg1, args...) = iscall(f) ? Symbol(repr(f)) : nameof(f)

# TODO: verify that this doesn't break other expressions that don't use DEC operators.

function SymbolicUtils.show_call(io, f, args)
  # replacing this version of the fname to include subscripts for DEC operators
  # fname = iscall(f) ? Symbol(repr(f)) : nameof(f)
  fname = nameof(f, symtype.(args)...)
  len_args = length(args)
  if Base.isunaryoperator(fname) && len_args == 1
      print(io, "$fname")
      print_arg(io, first(args), paren=true)
  elseif Base.isbinaryoperator(fname) && len_args > 1
      for (i, t) in enumerate(args)
          i != 1 && print(io, " $fname ")
          print_arg(io, t, paren=true)
      end
  else
      if issym(f)
          Base.show_unquoted(io, nameof(f))
      else
          Base.show_unquoted(io, fname)
      end
      print(io, "(")
      for i=1:length(args)
          print(io, args[i])
          i != length(args) && print(io, ", ")
      end
      print(io, ")")
  end
end


# TODO: conver these to tests using IOBuffer and string comparison
@syms x::PrimalForm{1,:X,2}
@syms y::PrimalForm{1,:X,2}
SymbolicUtils.show_term(stdout, Δ(x) + 2Δ(y))
println()
show_call(stdout, Δ, [x])
println()
nameof(Δ, symtype(x)) == :Δ₁