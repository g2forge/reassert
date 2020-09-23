package com.g2forge.reassert.express.eval;

import com.g2forge.reassert.express.model.IExpression;

public interface IEvaluator<Name, Value, Output> {
	public Output eval(IExpression<Name, Value> expression);
}
