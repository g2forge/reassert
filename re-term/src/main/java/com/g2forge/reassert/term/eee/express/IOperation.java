package com.g2forge.reassert.term.eee.express;

import java.util.List;

public interface IOperation<T> extends IExpression<T> {
	public List<? extends IExpression<T>> getArguments();
}
