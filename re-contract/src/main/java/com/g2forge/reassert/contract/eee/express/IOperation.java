package com.g2forge.reassert.contract.eee.express;

import java.util.List;

public interface IOperation<T> extends IExpression<T> {
	public List<? extends IExpression<T>> getArguments();
}
