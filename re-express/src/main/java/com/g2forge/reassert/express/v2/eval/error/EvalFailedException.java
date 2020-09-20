package com.g2forge.reassert.express.v2.eval.error;

import com.g2forge.reassert.express.v2.model.IExpression;

public class EvalFailedException extends ExpressionException {
	private static final long serialVersionUID = 8880981238203838258L;

	public EvalFailedException(IExpression<?, ?> expression) {
		super(expression, String.format("Failed to evaluate %1$s", expression));
	}
}
