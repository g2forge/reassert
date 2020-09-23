package com.g2forge.reassert.express.model;

public interface IExplainedValue<Value> extends IExplained<Value> {
	@Override
	public default Value get() {
		return getValue();
	}

	public Value getValue();
}
