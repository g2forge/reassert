package com.g2forge.reassert.express.model.variable;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.reassert.express.model.IExplained;

public interface IExplainedVariable<Name, Value> extends IExplained<Value> {
	@Override
	public default Value get() {
		return getExplained().get().get();
	}

	public IOptional<? extends IExplained<Value>> getExplained();

	public IVariable<Name, Value> getVariable();

	public default boolean isBound() {
		return !getExplained().isEmpty();
	}
}
