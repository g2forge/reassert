package com.g2forge.reassert.express.eval.error;

import com.g2forge.reassert.express.model.IExpression;

public interface IExpressionException {
	public IExpression<?, ?> getExpression();
}
