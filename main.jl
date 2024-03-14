function metajulia_eval(expr)
	if is_self_evaluating(expr)
		expr
	elseif expr isa Expr
		if is_call(expr)
			resolved_args = map(metajulia_eval, expr.args[2:end])
			call_symbol = expr.args[1]
			call_func = getfield(Base, call_symbol)

			call_func(resolved_args...)
		elseif is_or_operator(expr)
			metajulia_eval(expr.args[1]) || metajulia_eval(expr.args[2])
		elseif is_and_operator(expr)
			metajulia_eval(expr.args[1]) && metajulia_eval(expr.args[2])
		elseif is_if_statement(expr)
			if metajulia_eval(expr.args[1])
				metajulia_eval(expr.args[2])
			else
				metajulia_eval(expr.args[3])
			end
		elseif is_block(expr)
			# Filter out linenumbernodes
			lines = filter((arg) -> !(arg isa LineNumberNode), expr.args)
			resolved_lines = map(metajulia_eval, lines)
			resolved_lines[end]
		end
	else
		println("EVAL(", typeof(expr), "): Not implemented!")
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
    output = metajulia_eval(expr)
    println(output)
    repl()
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

## ????

function let_names(expr)
end
