package com.g2forge.reassert.contract.eee.evaluate;

import com.g2forge.reassert.contract.eee.express.IExpression;

public interface IEvaluator<T, R> {
	public R eval(IExpression<T> expression);
}
