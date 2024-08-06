using SymbolicUtils

struct DECVar{s} <: Number end
export DECVar

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

binop_dec = [:+, :-, :*, :∧]
for binop in binop_dec
  @eval begin
    @nospecialize
    function $binop(v::SymbolicUtils.BasicSymbolic{DECVar{s1}}, w::SymbolicUtils.BasicSymbolic{DECVar{s2}}) where {s1, s2}
      s′ = $binop(s1, s2)
      SymbolicUtils.Term{DECVar{s′}}($binop, [v, w])
    end

    export $binop
  end
end
