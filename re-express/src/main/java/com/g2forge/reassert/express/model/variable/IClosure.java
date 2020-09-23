package com.g2forge.reassert.express.model.variable;

import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.environment.IEnvironment;

public interface IClosure<Name, Value> extends IExpression<Name, Value> {
	public IEnvironment<Name, Value> getEnvironment();

	public IExpression<Name, Value> getExpression();
}
