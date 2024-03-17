const EMPTY_ENVIRONMENT = []

function metajulia_eval(expr, env)
	if is_self_evaluating(expr)
		expr
	elseif is_name(expr)
		eval_name(expr, env)
	elseif expr isa Expr
		if is_let(expr)
			eval_let(expr, env)
		elseif is_call(expr)
			resolved_args = map((arg) -> metajulia_eval(arg, env), expr.args[2:end])
			call_symbol = expr.args[1]
			call_func = getfield(Base, call_symbol)

			call_func(resolved_args...)
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
			lines = filter((arg) -> !(arg isa LineNumberNode), expr.args)
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
	assignments = expr.args[1].args
	names = map((assignment) -> assignment.args[1], assignments)
	values = map((assignment) -> metajulia_eval(assignment.args[2], env), assignments)
	extended_env = augment_environment(names, values, env)
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

# This could just be a dictionary :pensive:
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
	expr.head == :if
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
