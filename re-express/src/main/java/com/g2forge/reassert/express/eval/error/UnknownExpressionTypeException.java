package com.g2forge.reassert.express.eval.error;

import com.g2forge.reassert.express.model.IExpression;

public class UnknownExpressionTypeException extends ExpressionException {
	private static final long serialVersionUID = 4262598838218240643L;

	public UnknownExpressionTypeException(IExpression<?, ?> expression) {
		super(expression, String.format("Failed to evaluate %1$s", expression));
	}
}
