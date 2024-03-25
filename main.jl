const ENVIRONMENT = [Dict()]

function metajulia_eval(expr, env)
    if is_incomplete(expr)
        error("EVAL: Incomplete expression!")
    elseif is_dump(expr)
        println(env)
    elseif is_self_evaluating(expr)
        expr
    elseif is_name(expr)
        eval_name(expr, env)
    elseif is_quote(expr)
        eval_quote(expr, env)
    elseif is_let(expr)
        eval_let(expr, env)
    elseif is_anonymous_function(expr)
        eval_anonymous_function(expr, env)
    elseif is_assignment_var(expr)
        eval_assignment_var(expr, env)
    elseif is_assignment_function(expr)
        eval_assignment_typed(:function, expr, env)
    elseif is_assignment_fexpr(expr)
        eval_assignment_typed(:fexpr, expr, env)
    elseif is_assignment_macro(expr)
        eval_assignment_typed(:macro, expr, env)
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
        error("EVAL(", typeof(expr), "): Not implemented!")
        # error("Unknown expression type: $(typeof(expr))")
    end
end

function repl()
    print(">> ")
    input = readline(keep=true)
    expr = Meta.parse(input)
    while Meta.isexpr(expr, :incomplete)
        print("   ")
        input *= readline(keep=true)
        expr = Meta.parse(input)
    end
    if expr == :quit
        return
    end
    if isnothing(expr)
        repl()
    end

    output = metajulia_eval(expr, ENVIRONMENT)
    display(output)
    repl()
end

function display(value)
    if isnothing(value)
        println()
    elseif Meta.isexpr(value, :typed)
        println("<$(value.args[1])>")
    else
        println(value)
    end
end

## Environment ###################################
#region

function extend_environment(names, values, env)
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
    for i in eachindex(names)
        global_environment(env)[names[i]] = values[1]
    end
end

function augment_local(names, values, env)
    for i in eachindex(names)
        local_env = search_local(names[i], env)
        if isnothing(local_env)
            env[1][names[i]] = values[i]
        else
            local_env[names[i]] = values[i]
        end
    end
end

function augment_current(names, values, env)
    for i in eachindex(names)
        env[1][names[i]] = values[i]
    end
    env
end

function global_environment(env)
    env[end]
end

function search_env(name, env)
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
    # search all local envs
    for i in eachindex(env[1:end-1])
        if haskey(env[i], name)
            # return first env with name
            return env[i]
        end
    end
    # or nothing if name not defined
    nothing
end

#endregion

## Evals #########################################

#### Block

function eval_block(expr, env)
    # Filter out linenumbernodes
    lines = filter_linenumbernodes(expr.args)
    resolved_lines = map((line) -> metajulia_eval(line, env), lines)
    resolved_lines[end]
end

#### If statement

function eval_if_statement(expr, env)
    if metajulia_eval(expr.args[1], env)
        metajulia_eval(expr.args[2], env)
    else
        metajulia_eval(expr.args[3], env)
    end
end

#### Or (||)

function eval_or_operator(expr, env)
    metajulia_eval(expr.args[1], env) || metajulia_eval(expr.args[2], env)
end

#### And (&&)

function eval_and_operator(expr, env)
    metajulia_eval(expr.args[1], env) && metajulia_eval(expr.args[2], env)
end

#### Let

## TODO
function eval_let(expr, env)
    if Meta.isexpr(expr.args[1], :block)
        assignments = expr.args[1].args
    else
        assignments = [expr.args[1]]
    end

    extended_env = extend_environment([], [], env)
    if length(assignments) > 0 # TODO i think we can remove this

        for a in assignments
            # if function definition, dont eval the assignment
            # value (function block) TODO macros, fexprs
            if is_assignment_function(a)
                name = a.args[1].args[1]
                args = a.args[1].args[2:end]
                body = a.args[2]
                # Value to be saved (env = environment where function was defined)
                value = Expr(:typed, :function, args, body, env)
            else
                name = a.args[1]
                value = metajulia_eval(a.args[2], extended_env)
            end
            # extend environment for each assignment
            extended_env = augment_current([name], [value], extended_env)
        end
    end
    # let block
    metajulia_eval(expr.args[2], extended_env)
end

#### Name

function eval_name(name, env)
    name_env = search_env(name, env)
    if isnothing(name_env)
        error("Unbound name -- EVAL-NAME(", name, ")")
    end
    name_env[name]
end

#### Call

## TODO
function eval_call(expr, env)
    # Evaluate/Resolve the argument values
    values = expr.args[2:end]
    extended_env = copy(env) # Environment for the function block
    body = nothing # Function block (next expression to be evaluated)
    eval_args = true

    if is_anonymous_call(expr)
        # Anonymous function
        # Map argument symbols to argument values in function block environment
        arg_names = expr.args[1].args[1]
        if arg_names isa Symbol
            arg_names = [arg_names]
        else # Expression
            arg_names = arg_names.args
        end
        body = expr.args[1].args[2]
        func_env = extended_env
    else
        call_symbol = expr.args[1]
        # TODO handle call value being a symbol
        call_env = search_env(call_symbol, env)

        # if not in env
        if isnothing(call_env)
            # if not base function
            if !(call_symbol in names(Base))
                error("Unbound name -- EVAL-CALL(", call_symbol, ")")

            else # in Base
                values = map((arg) -> metajulia_eval(arg, env), values)
                call_func = getfield(Base, call_symbol)
                return call_func(values...)
            end

        else # in env
            # Map argument symbols to argument values in function block environment
            # call_value = Expr(:typed, type, args, block)
            call_value = call_env[call_symbol]
            type = call_value.args[1]
            arg_names = call_value.args[2]
            body = call_value.args[3]
            func_env = call_value.args[4]

            if type == :fexpr || type == :macro
                eval_args = false
            end
        end
    end

    if eval_args
        values = map((arg) -> metajulia_eval(arg, env), values)
        # if its an eval, remove quotes from arguments
        if call_symbol == :eval
            # also, find the environment that should be used for the evaluation
            eval_obj = search_env(Symbol("EVALENV"), env)[Symbol("EVALENV")]
            eval_env = (eval_obj.args[1]).args[1] # Kinda scuffed cause its quoted...
            values = unwrap_quote(values, eval_env)
        end
    else
        if type == :fexpr 
          # Save current environment (copy???) in case eval is present in fexpr body
          env_name = Symbol("EVALENV")
          env_value = Expr(:EVALENV, env)
          append!(arg_names, [env_name])
          append!(values, [env_value])
          values = wrap_quote(values)
        end
    end

    # function call extends environment where it was created
    extended_env = extend_environment(arg_names, values, func_env)
    
    # Evaluate the function block with the extended environment
    res = metajulia_eval(body, extended_env)
    if (type == :macro)
        if Meta.isexpr(res, :quote)
            # Evaluate macro body once more, now on parent environment
            return metajulia_eval(res.args[1], env) # TODO [1] ok?? :)
        else
            return res.value
        end
    end
    res
end

#### Assignment

function make_assignment(name, value, env)
    augment_environment([name], [value], env)
    value
end

# Doesn't evaluate right side of assignment (functions, macros)
# TODO probably need to further separate macros special things
function eval_assignment_typed(type, expr, env)
    name = expr.args[1].args[1]
    args = expr.args[1].args[2:end]
    body = expr.args[2]
    # Value to be saved (env = environment where function was defined)
    value = Expr(:typed, type, args, body, env)
    if type == :fexpr
        value.args[4] = extend_fexpr_eval(env)
    end
    # Do the assignment
    make_assignment(name, value, env)
end

function extend_fexpr_eval(env)
    # Environment with eval (fexpr!)
    eval_name = :eval
    eval_value = Expr(:typed, :function, [:expr], Expr(:block, :expr), env)
    extend_environment([eval_name], [eval_value], env)
end

# Evaluate assignment for a variable (right side isn't a function body)
function eval_assignment_var(expr, env)
    name = expr.args[1]
    rightSide = expr.args[2]
    # Not a function definition, evaluate the right side
    value = metajulia_eval(rightSide, env)
    # Do the assignment
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

    value = metajulia_eval(expr, env)
    make_assignment(name, value, [global_environment(env)])
end

### Anonymous Function

function eval_anonymous_function(expr, env)
    args = expr.args[1] isa Symbol ? [expr.args[1]] : expr.args[1].args
    body = expr.args[2]
    Expr(:typed, :function, args, body, env)
end

#### Quote

# This is really ugly and maybe we can just ditch QuoteNodes
function eval_quote(expr, env)
    new_expr = expr
    if expr isa Expr
        new_expr = copy(expr)
        new_expr.args = map((arg) -> Meta.isexpr(arg, :$) ? metajulia_eval(arg.args[1], env)
                                 : (arg isa QuoteNode ? eval_quote_node(arg) : eval_quote(arg, env)), expr.args)
    elseif expr isa QuoteNode
        return eval_quote_node(expr)
    end
    new_expr
end

function eval_quote_node(node)
    is_self_evaluating(node.value) ? node.value : node
end

## Type tests #########################################

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

function is_global(expr)
    Meta.isexpr(expr, :global)    
end

function is_dump(expr)
    expr == :dump
end

function is_quit(expr)
    expr == :quit
end

## Helper Functions ##############################

function filter_linenumbernodes(args)
    # Filter out linenumbernodes
    filter((arg) -> !(arg isa LineNumberNode), args)
end

function wrap_quote(args)
    map((arg) -> arg isa Symbol ? QuoteNode(arg) : Expr(:quote, arg), args)
end

function unwrap_quote(args, env)
    new_args = map((arg) -> arg isa QuoteNode ? arg.value : arg.args[1], args) # ?? TODO [1] ok?
    map((arg) -> metajulia_eval(arg, env), new_args)
end

# Initialize environment with primitives yadda yadda
function init_env(env)
    names = []
    values = []
    # Augment global environment
    augment_global(names, values, env)
end
init_env(ENVIRONMENT)

repl()
