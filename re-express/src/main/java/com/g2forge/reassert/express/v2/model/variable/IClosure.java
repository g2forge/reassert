package com.g2forge.reassert.express.v2.model.variable;

import com.g2forge.reassert.express.v2.model.IExpression;

public interface IClosure<Name, Value> extends IExpression<Name, Value> {
	public IEnvironment<Name, Value> getEnvironment();

	public IExpression<Name, Value> getExpression();
}
