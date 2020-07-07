package com.g2forge.reassert.term.eee.evaluate;

import com.g2forge.reassert.term.eee.express.IExpression;

public interface IEvaluator<T, R> {
	public R eval(IExpression<T> expression);
}
