package com.g2forge.reassert.expression.express;

import com.g2forge.alexandria.java.adt.name.INamed;
import com.g2forge.reassert.expression.explain.model.IExplained;

public interface IConstant<T> extends IExpression<T>, IExplained<T>, INamed<Object> {
	public T getValue();
}
