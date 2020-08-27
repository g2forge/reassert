package com.g2forge.reassert.expression.evaluate;

import com.g2forge.reassert.expression.express.IExpression;

public interface IEvaluator<T, R> {
	public R eval(IExpression<T> expression);
}
