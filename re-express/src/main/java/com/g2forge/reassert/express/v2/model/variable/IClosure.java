package com.g2forge.reassert.express.v2.model.variable;

import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.environment.IEnvironment;

public interface IClosure<Name, Value> extends IExpression<Name, Value> {
	public IEnvironment<Name, Value> getEnvironment();

	public IExpression<Name, Value> getExpression();
}