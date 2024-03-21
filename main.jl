const EMPTY_ENVIRONMENT = []

function metajulia_eval(expr, env)
	if is_incomplete(expr)
		error("EVAL: Incomplete expression!")
	elseif is_quit(expr)
		nothing
	elseif is_dump(expr)
		println(dump(env))
	elseif is_self_evaluating(expr)
		expr
	elseif is_name(expr)
		eval_name(expr, env)[2]
	elseif expr isa Expr
		if is_let(expr)
			eval_let(expr, env)
		elseif is_assignment(expr)
			eval_assignment(expr, env)
		elseif is_call(expr)
			eval_call(expr, env)
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
	output = metajulia_eval(expr, EMPTY_ENVIRONMENT)
  println(output)
  repl()
end

## Evals

#### Let

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
			# if function definition, dont eval the assignment value (function block)
			value = is_function_definition(a) ? a.args[2] : metajulia_eval(a.args[2], extended_env)
			# extend environment for each assignment
			extended_env = augment_environment([name], [value], extended_env)
		end

		# names = map((assignment) -> assignment.args[1], assignments)
		# # if function definition, dont eval the assignment value (function block)
		# values = map((assignment) -> is_function_definition(assignment) ?
		# 	assignment.args[2] : metajulia_eval(assignment.args[2], env), assignments)
		# # use new environment with let bindings and which covers let block (for its own assignments)
		# extended_env = augment_environment(names, values, extended_env)
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
	# was getting some weird error with oob access so moved the [2] up
  env[get_name_index(name, env)]
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

function eval_call(expr, env)
	# Evaluate/Resolve the argument values
	values = map((arg) -> metajulia_eval(arg, env), expr.args[2:end])
	extended_env = copy(env) # Environment for the function block
	next_expr = nothing # Function block (next expression to be evaluated)

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
			# Try to find the function in base Julia
			call_func = getfield(Base, call_symbol)
			# If no error thrown, function exists, just return the value
			return call_func(values...)
		catch
			# if !(e isa UndefVarError)
			# 	rethrow(e)
			# end

			# Try to find the function definition in the environment
      # (name . (args/names, block/next_expr))
      call_pair = env[get_name_index(call_symbol, env)][2]
			# Map argument symbols to argument values in function block environment
      names = call_pair[1]
      next_expr = call_pair[2]
		end
	end

	if length(names) > 0
		# Function has arguments and so the env needs to be augmented
		extended_env = augment_environment(names, values, extended_env)
	end
	# Evaluate the function block with the extended environment
	metajulia_eval(next_expr, extended_env)
end

#### Assignment

function eval_assignment(expr, env)
	name = expr.args[1]
	value = expr.args[2]
  ret = nothing
	is_function_def = is_function_definition(expr)
	
  try
    var = nothing
    if !is_function_def
      # Set return to right side value
      ret = value
	  	# Not a function definition, evaluate the right side
	  	value = metajulia_eval(value, env)
    else
	    # Set return to <function>
      ret = "<function>"
      name = expr.args[1].args[1]
      args = expr.args[1].args[2:end]
      # Value to be saved will be pair (args, func_body)
      value = (args, value)
	  end
	  # Search for the name in the environment
    var = get_name_index(name, env)
	  # Found! Update the value of the variable in the environment
    env[var] = (name, value)
  catch
	  # If the name is not found, add it to the environment
	  # Destructively? change the environment
	  augment_environment([name], [value], env)
  end
  ret
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

function is_anonymous_function(expr)
	Meta.isexpr(expr.args[1], :->)
end

function is_assignment(expr)
	Meta.isexpr(expr, :(=))
end

function is_dump(expr)
	expr == :dump
end

function is_quit(expr)
	expr == :quit
end

## Helper Functions

function filter_linenumbernodes(args)
	# Filter out linenumbernodes
	filter((arg) -> !(arg isa LineNumberNode), args)
end

repl()
