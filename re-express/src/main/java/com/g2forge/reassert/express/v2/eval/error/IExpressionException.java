package com.g2forge.reassert.express.v2.eval.error;

import com.g2forge.reassert.express.v2.model.IExpression;

public interface IExpressionException {
	public IExpression<?, ?> getExpression();
}
