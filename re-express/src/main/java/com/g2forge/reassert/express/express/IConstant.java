package com.g2forge.reassert.express.express;

import com.g2forge.alexandria.java.adt.name.INamed;
import com.g2forge.reassert.express.explain.model.IExplained;

public interface IConstant<T> extends IExpression<T>, IExplained<T>, INamed<Object> {
	public T getValue();
}
