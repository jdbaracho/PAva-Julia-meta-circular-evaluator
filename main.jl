### Structs ####################################################################

struct Function
    args::Array{Any}
    body::Any
    env::Any
end
Base.show(io::IO, s::Function) = print(io::IO, "<function>")

mutable struct Eval
    args::Array{Any}
    body::Any
    env::Any
end
Base.show(io::IO, s::Eval) = print(io::IO, "<function>")

struct Fexpr
    args::Array{Any}
    body::Any
    env::Any
end
Base.show(io::IO, s::Fexpr) = print(io::IO, "<fexpr>")

struct Macro
    args::Array{Any}
    body::Any
    env::Any
end
Base.show(io::IO, s::Macro) = print(io::IO, "<macro>")

## Repl ########################################################################

function eval(expr, env)
    if is_incomplete(expr)
        error("Incomplete expression -- EVAL-EXPR(", expr, ")")
    elseif is_self_evaluating(expr)
        expr
    elseif is_name(expr)
        eval_name(expr, env)
    elseif is_quote(expr)
        eval_quote(expr, true, env)
    elseif is_let(expr)
        eval_let(expr, env)
    elseif is_anonymous_function(expr)
        eval_anonymous_function(expr, env)
    elseif is_assignment_var(expr)
        eval_assignment_var(expr, env)
    elseif is_assignment_function(expr)
        eval_assignment_function(expr, env)
    elseif is_assignment_fexpr(expr)
        eval_assignment_fexpr(expr, env)
    elseif is_assignment_macro(expr)
        eval_assignment_macro(expr, env)
    elseif is_global(expr)
        eval_global(expr, env)
    elseif is_call(expr)
        eval_call(expr, env)
    elseif is_or_operator(expr)
        eval_or_operator(expr, env)
    elseif is_and_operator(expr)
        eval_and_operator(expr, env)
    elseif is_if_statement(expr)
        eval_if_statement(expr, env)
    elseif is_block(expr)
        eval_block(expr, env)
    else
        error("Not implemented -- EVAL(", typeof(expr), ")")
    end
end

function metajulia_eval(expr)
    eval(expr, clean_environment())
end

function repl()
    env = clean_environment()

    while true
        expr = read_input()
        expr isa Nothing ? continue :
        quit(expr) ? break :
        value = eval(expr, env)
        show(value)
        println()
    end
end

function quit(expr)
    expr == :quit
end

function read_input()
    print(">> ")
    input = readline(keep=true)
    expr = Meta.parse(input)
    while Meta.isexpr(expr, :incomplete)
        print('\t')
        input *= readline(keep=true)
        expr = Meta.parse(input)
    end
    expr
end

function clean_environment()
    [Dict()]
end

## Environment #################################################################

function extend_environment(names, values, env)
    # extend current environment
    new_env = copy(env)
    local_env = Dict(names[i] => values[i] for i in eachindex(names))
    pushfirst!(new_env, local_env)
end

function augment_environment(names, values, env)
    if length(env) == 1
        augment_global(names, values, env)
    else
        augment_local(names, values, env)
    end
end

function augment_global(names, values, env)
    # add name to global environment
    for i in eachindex(names)
        global_environment(env)[names[i]] = values[1]
    end
end

function augment_local(names, values, env)
    # update name if it exists, add name if not
    for i in eachindex(names)
        local_env = search_local(names[i], env)
        if isnothing(local_env)
            env[1][names[i]] = values[i]
        else
            local_env[names[i]] = values[i]
        end
    end
end

function global_environment(env)
    env[end]
end

function search_env(name, env)
    # return environment where name is defined
    name_env = search_local(name, env)
    if isnothing(name_env)
        name_env = search_global(name, env)
    end
    name_env
end

function search_global(name, env)
    global_env = global_environment(env)
    if haskey(global_env, name)
        global_env
    else
        nothing
    end
end

function search_local(name, env)
    # search all local environments
    for i in eachindex(env[1:end-1])
        if haskey(env[i], name)
            # return first environment with name
            return env[i]
        end
    end
    # or nothing if name not defined
    nothing
end

## Evals #######################################################################

## Block

function eval_block(expr, env)
    # filter out linenumbernodes
    lines = filter_linenumbernodes(expr.args)
    resolved_lines = map((line) -> eval(line, env), lines)
    resolved_lines[end]
end

function filter_linenumbernodes(args)
    # filter out linenumbernodes
    filter((arg) -> !(arg isa LineNumberNode), args)
end

## If statement

function eval_if_statement(expr, env)
    if eval(expr.args[1], env)
        eval(expr.args[2], env)
    else
        eval(expr.args[3], env)
    end
end

## Or (||)

function eval_or_operator(expr, env)
    eval(expr.args[1], env) || eval(expr.args[2], env)
end

## And (&&)

function eval_and_operator(expr, env)
    eval(expr.args[1], env) && eval(expr.args[2], env)
end

## Let

function eval_let(expr, env)
    if Meta.isexpr(expr.args[1], :block)
        assignments = expr.args[1].args
    else
        assignments = [expr.args[1]]
    end

    extended_env = extend_environment([], [], env)
    for a in assignments
        if is_assignment_function(a)
            eval_assignment_function(a, extended_env)
        elseif is_assignment_fexpr(a)
            eval_assignment_fexpr(a, extended_env)
        elseif is_assignment_macro(a)
            eval_assignment_macro(a, extended_env)
        elseif is_assignment_var(a)
            eval_assignment_var(a, extended_env)
        else
            error("Expected assignment but got expression of
             type(", typeof(a), ") -- EVAL-LET(", expr, ")")
        end
    end
    # let body
    eval(expr.args[2], extended_env)
end

## Name

function eval_name(name, env)
    name_env = search_env(name, env)
    if isnothing(name_env)
        error("Unbound name -- EVAL-NAME(", name, ")")
    end
    name_env[name]
end

## Call

function eval_call(expr, env)
    values = expr.args[2:end]

    if is_anonymous_call(expr)
        call_value = eval_call_anonymous(expr, env)
    else
        call_symbol = expr.args[1]
        call_env = search_env(call_symbol, env)

        # if not in env, then attempt to use base julia
        if isnothing(call_env)
            return eval_call_base(call_symbol, values, env)
        end # in env

        call_value = call_env[call_symbol]
    end

    arg_names = copy(call_value.args)
    body = call_value.body
    func_env = call_value.env

    if call_value isa Function
        eval_call_function(arg_names, body, func_env, values, env, false)
    elseif call_value isa Eval
        eval_call_function(arg_names, body, func_env, values, env, true)
    elseif call_value isa Fexpr
        eval_call_fexpr(arg_names, body, func_env, values, env)
    elseif call_value isa Macro
        eval_call_macro(arg_names, body, func_env, values, env)
    end
end

function eval_call_macro(arg_names, body, func_env, values, env)
    # function call extends environment where it was created
    extended_env = extend_environment(arg_names, values, func_env)
    # evaluate the function block with the extended environment
    # and eval the result
    eval(eval(body, extended_env), env)
end

function eval_call_fexpr(arg_names, body, func_env, values, env)
    # function call extends environment where it was created
    extended_env = extend_environment(arg_names, values, func_env)
    augment_fexpr_eval(extended_env)

    # save environment where eval was called to evaluate arguments
    eval_obj = search_env(:eval, extended_env)[:eval]
    eval_obj.env = env

    # evaluate the function block with the extended environment
    eval(body, extended_env)
end

function augment_fexpr_eval(env)
    # add eval to the fexpr environment
    eval_name = :eval
    eval_value = Eval([:x], :x, env)
    augment_environment([eval_name], [eval_value], env)
end

function eval_call_function(arg_names, body, func_env, values, env, is_eval)
    # evaluate arguments
    values = map((arg) -> eval(arg, env), values)
    if is_eval
        values = map((arg) -> eval(arg, func_env), values)
    end

    # function call extends environment where it was created
    extended_env = extend_environment(arg_names, values, func_env)
    # Evaluate the function block with the extended environment
    eval(body, extended_env)
end

function eval_call_base(call_symbol, values, env)
    # if not base function
    if !(call_symbol in names(Base))
        error("Unbound name -- EVAL-CALL(", call_symbol, ")")

    else # in Base
        # evaluate arguments
        values = map((arg) -> eval(arg, env), values)
        call_func = getfield(Base, call_symbol)
        return call_func(values...)
    end
end

function eval_call_anonymous(expr, env)
    # Anonymous function
    arg_names = expr.args[1].args[1]
    if arg_names isa Symbol
        arg_names = [arg_names]
    else # Expression
        arg_names = arg_names.args
    end
    body = expr.args[1].args[2]
    func_env = copy(env)

    Function(arg_names, body, func_env)
end

## Assignment

function make_assignment(name, value, env)
    augment_environment([name], [value], env)
    value
end

function eval_assignment_function(expr, env)
    name = expr.args[1].args[1]
    args = expr.args[1].args[2:end]
    body = expr.args[2]
    # save environment where function was defined
    value = Function(args, body, env)
    make_assignment(name, value, env)
end

function eval_assignment_fexpr(expr, env)
    name = expr.args[1].args[1]
    args = expr.args[1].args[2:end]
    body = expr.args[2]
    # save environment where function was defined
    value = Fexpr(args, body, env)
    make_assignment(name, value, env)
end

function eval_assignment_macro(expr, env)
    name = expr.args[1].args[1]
    args = expr.args[1].args[2:end]
    body = expr.args[2]
    # save environment where function was defined
    value = Macro(args, body, env)
    make_assignment(name, value, env)
end

function eval_assignment_var(expr, env)
    # Evaluate assignment for a variable
    name = expr.args[1]
    right_side = expr.args[2]
    # Not a function definition, evaluate the right side
    value = eval(right_side, env)
    make_assignment(name, value, env)
end

function eval_global(expr, env)
    # remove global
    expr = expr.args[1]

    assignemnt = expr.args[1]
    if assignemnt isa Expr
        name = assignemnt.args[1]
    else # var assignment
        name = assignemnt
    end

    value = eval(expr, env)
    make_assignment(name, value, [global_environment(env)])
end

## Anonymous Function

function eval_anonymous_function(expr, env)
    args = expr.args[1] isa Symbol ? [expr.args[1]] : expr.args[1].args
    body = expr.args[2]
    Function(args, body, env)
end

## Quote

function eval_quote(expr, force, env)
    new_expr = expr
    if expr isa Expr
        new_expr = copy(expr)
        new_expr.args = map((arg) -> Meta.isexpr(arg, :$) ? eval(arg.args[1], env)
                                     : (arg isa QuoteNode ? eval_quote_node(arg) : eval_quote(arg, false, env)), expr.args)
    end
    if is_quote_node(new_expr)
        return eval_quote_node(new_expr)
    end
    if force
        return new_expr.args[1]
    end
    new_expr
end

function eval_quote_node(node)
    if node isa QuoteNode
        (is_self_evaluating(node.value) || node.value isa Symbol) ? node.value : node
    else
        node.args[1]
    end
end

## Type tests ##################################################################

function is_self_evaluating(expr)
    expr isa Number ||
        expr isa String ||
        expr isa Bool
end

function is_call(expr)
    Meta.isexpr(expr, :call)
end

function is_or_operator(expr)
    Meta.isexpr(expr, :||)
end

function is_and_operator(expr)
    Meta.isexpr(expr, :&&)
end

function is_if_statement(expr)
    Meta.isexpr(expr, :if) || Meta.isexpr(expr, :elseif)
end

function is_block(expr)
    Meta.isexpr(expr, :block)
end

function is_name(expr)
    expr isa Symbol
end

function is_let(expr)
    Meta.isexpr(expr, :let)
end

function is_incomplete(expr)
    Meta.isexpr(expr, :incomplete)
end

function is_anonymous_function(expr)
    Meta.isexpr(expr, :->)
end

function is_anonymous_call(expr)
    Meta.isexpr(expr.args[1], :->)
end

function is_assignment_function(expr)
    Meta.isexpr(expr, :(=)) && Meta.isexpr(expr.args[1], :call) && Meta.isexpr(expr.args[2], :block)
end

function is_assignment_var(expr)
    Meta.isexpr(expr, :(=)) && !is_assignment_function(expr)
end

function is_assignment_fexpr(expr)
    Meta.isexpr(expr, Symbol(":="))
end

function is_assignment_macro(expr)
    Meta.isexpr(expr, Symbol("\$="))
end

function is_quote(expr)
    expr isa QuoteNode || Meta.isexpr(expr, :quote)
end

function is_quote_node(expr)
    expr isa QuoteNode || Meta.isexpr(expr, :quote) && length(expr.args) == 1 &&
                              (is_self_evaluating(expr.args[1]) || expr.args[1] isa Symbol)
end

function is_global(expr)
    Meta.isexpr(expr, :global)
end
