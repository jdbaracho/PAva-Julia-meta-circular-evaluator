const EMPTY_ENVIRONMENT = []

function metajulia_eval(expr, env)
    if is_incomplete(expr)
        error("EVAL: Incomplete expression!")
    elseif is_dump(expr)
        println(dump(env))
    elseif is_self_evaluating(expr)
        expr
    elseif is_name(expr)
        eval_name(expr, env)
    elseif is_quote(expr)
        eval_quote(expr, 2, env)
    elseif is_let(expr)
        eval_let(expr, env)
    elseif is_assignment_var(expr)
        eval_assignment_var(expr, env)
    elseif is_assignment_function(expr)
        eval_assignment_typed(:function, expr, env)
    elseif is_assignment_fexpr(expr)
        eval_assignment_typed(:fexpr, expr, env)
    elseif is_assignment_macro(expr)
        eval_assignment_typed(:macro, expr, env)
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

    output = metajulia_eval(expr, EMPTY_ENVIRONMENT)
    println(output)
    repl()
end

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

    extended_env = copy(env)
    if length(assignments) > 0

        for a in assignments
            name = a.args[1]
            # if function definition, dont eval the assignment value (function block) TODO macros, fexprs
            value = is_assignment_function(a) ? a.args[2] : metajulia_eval(a.args[2], extended_env)
            # extend environment for each assignment
            extended_env = augment_environment([name], [value], extended_env)
        end
    end
    # let block
    metajulia_eval(expr.args[2], extended_env)
end

function augment_environment(names, values, env)
    if length(names) > 1
        extended_env = augment_environment(names[2:end], values[2:end], env)
    else
        extended_env = env
    end
    pushfirst!(extended_env, (names[1], values[1]))
end

#### Name

function eval_name(name, env)
    obj = env[get_name_index(name, env)][2]
    if Meta.isexpr(obj, :typed)
        "<$(obj.args[1])>" # TODO use structs to make generic method to get output Base.show(io::IO, ...
    else
        obj
    end
end

# Gets index of name in env
function get_name_index(name, env)
    if isempty(env)
        error("Unbound name -- EVAL-NAME(", name, ")")
    elseif first(env)[1] == name
        1
    else
        get_name_index(name, env[2:end]) + 1
    end
end

#### Call

## TODO
function eval_call(expr, env)
    # Evaluate/Resolve the argument values
    values = expr.args[2:end]
    extended_env = copy(env) # Environment for the function block
    next_expr = nothing # Function block (next expression to be evaluated)
    eval_args = true

    if is_anonymous_function(expr)
        # Anonymous function
        # Map argument symbols to argument values in function block environment
        names = expr.args[1].args[1]
        if names isa Symbol
            names = [names]
        else # Expression
            names = names.args
        end
        next_expr = expr.args[1].args[2]
    else
        call_symbol = expr.args[1]
        try
            # Try to find the function definition in the environment
            # (name . (args/names, block/next_expr)) ???? TODO
            call_obj = Symbol(nothing)
            while (call_obj isa Symbol)
                call_obj = env[get_name_index(call_symbol, env)][2]
            end
            # call_pair = env[get_name_index(call_symbol, env)][2]
            # Map argument symbols to argument values in function block environment
            names = call_obj.args[1]
            next_expr = call_obj.args[2]

            if Meta.isexpr(call_obj, :fexpr)
                eval_args = false
            end
        catch
            values = map((arg) -> metajulia_eval(arg, env), values)
            # Try to find the function in base Julia
            call_func = getfield(Base, call_symbol)
            # If no error thrown, function exists, just return the value
            return call_func(values...)
        end
    end

    if eval_args
        values = map((arg) -> metajulia_eval(arg, env), values)
    else
        values = wrap_quote(values)
    end

    if length(names) > 0
        # Function has arguments and so the env needs to be augmented
        extended_env = augment_environment(names, values, extended_env)
    end
    # Evaluate the function block with the extended environment
    metajulia_eval(next_expr, extended_env)
end

#### Assignment

function make_assignment(name, value, env)
    try
        # Search for the name in the environment
        var = get_name_index(name, env)
        # Found! Update the value of the variable in the environment
        env[var] = (name, value)
    catch
        # If the name is not found, add it to the environment
        # Destructively? change the environment
        augment_environment([name], [value], env)
    end
    # Return type or value
    if Meta.isexpr(value, :typed)
        return "<$(value.args[1])>"
    end
    value
end

# Doesn't evaluate right side of assignment (functions, fexprs, macros)
# TODO probably need to further separate macros and fexprs special things
function eval_assignment_typed(type, expr, env)
    name = expr.args[1].args[1]
    args = expr.args[1].args[2:end]
    body = expr.args[2]
    # Value to be saved
    value = Expr(:typed, type, args, body)
    # Do the assignment
    make_assignment(name, value, env)
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

#### Quote

# This is really ugly and maybe we can just ditch QuoteNodes
function eval_quote(expr, depth, env)
    if depth > 0
        if expr isa Expr
            expr.args = map((arg) -> Meta.isexpr(arg, :$) ? metajulia_eval(arg.args[1], env)
                                     : (arg isa QuoteNode ? eval_quote_node(expr) : eval_quote(arg, depth - 1, env)), expr.args)
        elseif expr isa QuoteNode
            return eval_quote_node(expr)
        end
    end
    expr
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

function is_dump(expr)
    expr == :dump
end

function is_quit(expr)
    expr == :quit
end

## Helper Functions #########################################

function filter_linenumbernodes(args)
    # Filter out linenumbernodes
    filter((arg) -> !(arg isa LineNumberNode), args)
end

function wrap_quote(args)
    map((arg) -> arg isa Symbol ? QuoteNode(arg) : Expr(:quote, arg), args)
end

repl()
