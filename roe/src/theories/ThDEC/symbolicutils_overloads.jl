using SymbolicUtils
using ...DEC

unop_dec = [:∂ₜ, :d, :★, :♯, :♭, :-]
for unop in unop_dec
    @eval begin
        @nospecialize
        function $unop(
            v::SymbolicUtils.BasicSymbolic{OfSort{s}}
        ) where {s}
            s′ = $unop(s)
            SymbolicUtils.Term{OfSort{s′}}($unop, [v])
        end

        export $unop
    end
end

binop_dec = [:+, :-, :*, :∧]
for binop in binop_dec
    @eval begin
        @nospecialize
        function $binop(
            v::SymbolicUtils.BasicSymbolic{OfSort{s1}},
            w::SymbolicUtils.BasicSymbolic{OfSort{s2}}
        ) where {s1,s2}
            s′ = $binop(s1, s2)
            SymbolicUtils.Term{OfSort{s′}}($binop, [v, w])
        end

        export $binop
    end
end
