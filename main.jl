function meta_eval(expr, env)
    if is_self_evaluating(expr)
        expr
    elseif is_name(expr)
        println(">> name")
        #eval_name(expr, env)
    elseif is_let(expr)
        println(">> let")
        #eval_let(exp, env)
    else
        throw(error("Unknown expression type -- EVAL $(expr)"))
    end
end

function is_self_evaluating(expr)
    expr isa Number ||
    expr isa String ||
    expr isa Bool
end

function is_name(expr)
    expr isa Symbol
end

function is_let(expr)
    expr isa Expr && expr.head == :let
end

