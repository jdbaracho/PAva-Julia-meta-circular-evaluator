function metajulia_eval(expr, env)
    if is_self_evaluating(expr)
        expr
    elseif is_name(expr)
        #eval_name(expr, env)
    elseif is_let(expr)
        #eval_let(exp, env)
    else
        throw(error("Unknown expression type -- EVAL $(expr)"))
    end
end

function repl()
    print(">> ")
    input = readline()
    expr = Meta.parse(input)
    while Meta.isexpr(expr, :incomplete)
        input *= readline()
        expr = Meta.parse(input)
    end
    output = metajulia_eval(expr, [])
    println(output)
    repl()
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
    Meta.isexpr(expr, :let)
end

function let_names(expr)
end

