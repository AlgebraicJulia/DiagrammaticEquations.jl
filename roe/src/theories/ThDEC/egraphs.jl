## SIGNATURE

# Predicates
function isForm(g, ec::EClass)
  any(ec.nodes) do n
    h = v_head(n)
    if has_constant(g, h)
      c = get_constant(g, h)
      return c isa Form
    end
    false
  end
end


