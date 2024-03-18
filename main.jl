const EMPTY_ENVIRONMENT = []

function metajulia_eval(expr, env)
	if is_incomplete(expr)
		error("EVAL: Incomplete expression!")
	elseif is_self_evaluating(expr)
		expr
	elseif is_name(expr)
		eval_name(expr, env)
	elseif expr isa Expr
		if is_let(expr)
			eval_let(expr, env)
		elseif is_call(expr)
			# Evaluate/Resolve the argument values
			resolved_args = map((arg) -> metajulia_eval(arg, env), expr.args[2:end])
			call_symbol = expr.args[1]
			try
				# Try to find the function in base Julia
				call_func = getfield(Base, call_symbol)
				call_func(resolved_args...)
			catch e
				if !(e isa UndefVarError)
					rethrow(e)
				end
				# Try to find the function definition in the environment
				call_pair = get_custom_function(call_symbol, env)
				# Map argument symbols to argument values (resolved_args) in function block environment
				names = call_pair[1].args[2:end]
				values = resolved_args
				extended_env = augment_environment(names, values, env)
				# Evaluate the function block with the extended environment
				metajulia_eval(call_pair[2], extended_env)
			end
		elseif is_or_operator(expr)
			metajulia_eval(expr.args[1], env) || metajulia_eval(expr.args[2], env)
		elseif is_and_operator(expr)
			metajulia_eval(expr.args[1], env) && metajulia_eval(expr.args[2], env)
		elseif is_if_statement(expr)
			if metajulia_eval(expr.args[1], env)
				metajulia_eval(expr.args[2], env)
			else
				metajulia_eval(expr.args[3], env)
			end
		elseif is_block(expr)
			# Filter out linenumbernodes
			lines = filter_linenumbernodes(expr.args)
			resolved_lines = map((line) -> metajulia_eval(line, env), lines)
			resolved_lines[end]
		end
	else
		error("EVAL(", typeof(expr), "): Not implemented!")
		# error("Unknown expression type: $(typeof(expr))")
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
	output = metajulia_eval(expr, EMPTY_ENVIRONMENT)
    println(output)
    repl()
end

## Evals

function eval_let(expr, env)
	if Meta.isexpr(expr.args[1], :block)
		assignments = expr.args[1].args
	else
		assignments = [expr.args[1]]
	end

	names = map((assignment) -> assignment.args[1], assignments)
	# if function definition, dont eval the assignment value (function block)
	values = map((assignment) -> is_function_definition(assignment) ?
		assignment.args[2] : metajulia_eval(assignment.args[2], env), assignments)
	extended_env = augment_environment(names, values, env)
	# let block
	metajulia_eval(expr.args[2], extended_env)
end

function augment_environment(names, values, env)
	if length(names) > 1
		extended_env = augment_environment(names[2:end], values[2:end], env)
	else 
		extended_env = copy(env)
	end
	pushfirst!(extended_env, (names[1], values[1]))
end

function eval_name(name, env)
	if isempty(env)
		error("Unbound name -- EVAL-NAME", name)
	elseif first(env)[1] == name
		first(env)[2]
	else
		eval_name(name, env[2:end])
	end
end

## Type tests

function is_self_evaluating(expr)
    expr isa Number ||
    expr isa String ||
    expr isa Bool
end

function is_call(expr)
	expr.head == :call
end

function is_or_operator(expr)
	expr.head == :||
end

function is_and_operator(expr)
	expr.head == :&&
end

function is_if_statement(expr)
	expr.head == :if || expr.head == :elseif 
end

function is_block(expr)
	expr.head == :block
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

function is_function_definition(expr)
	Meta.isexpr(expr, :(=)) && Meta.isexpr(expr.args[1], :call) && Meta.isexpr(expr.args[2], :block)
end

function filter_linenumbernodes(args)
	# Filter out linenumbernodes
	filter((arg) -> !(arg isa LineNumberNode), args)
end

function get_custom_function(name, env)
	if isempty(env)
		error("Unbound name -- GET-CUSTOM-FUNCTION", name)
	else
		# Retrieve first pair (signature, block)
		curr = first(env)
		if Meta.isexpr(curr[1], :call) && curr[1].args[1] == name
			# Found function definition
			curr
		else
			get_custom_function(name, env[2:end])
		end
	end
end
