package com.g2forge.reassert.express.eval.error;

import com.g2forge.reassert.express.model.IExpression;

import lombok.Getter;

@Getter
public class ExpressionException extends RuntimeException implements IExpressionException {
	private static final long serialVersionUID = -2659881980109741928L;

	protected final IExpression<?, ?> expression;

	public ExpressionException(IExpression<?, ?> expression) {
		super(expression.toString());
		this.expression = expression;
	}

	protected ExpressionException(IExpression<?, ?> expression, String message) {
		super(message);
		this.expression = expression;
	}

	protected ExpressionException(IExpression<?, ?> expression, String message, Throwable cause) {
		super(message, cause);
		this.expression = expression;
	}
}
