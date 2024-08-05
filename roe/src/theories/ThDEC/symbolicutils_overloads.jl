using SymbolicUtils

struct DECVar{s} <: Number
end

unop_dec = [:∂ₜ, :d, :★, :♯, :♭, :-]
for unop in unop_dec
  @eval begin
    @nospecialize
    function $unop(v::SymbolicUtils.BasicSymbolic{DECVar{s}}) where s
      s′ = $unop(s)
      SymbolicUtils.Term{DECVar{s′}}($unop, [v])
    end

    export $unop
  end
end
