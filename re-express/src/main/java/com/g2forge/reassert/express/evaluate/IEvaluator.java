package com.g2forge.reassert.express.evaluate;

import com.g2forge.reassert.express.express.IExpression;

public interface IEvaluator<T, R> {
	public R eval(IExpression<T> expression);
}
