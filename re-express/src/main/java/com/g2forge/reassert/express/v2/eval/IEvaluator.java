package com.g2forge.reassert.express.v2.eval;

import com.g2forge.reassert.express.v2.model.IExpression;

public interface IEvaluator<Name, Value, Output> {
	public Output eval(IExpression<Name, Value> expression);
}
