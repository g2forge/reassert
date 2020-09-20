package com.g2forge.reassert.express.v2.model;

public interface IExplainedValue<Value> extends IExplained<Value> {
	@Override
	public default Value get() {
		return getValue();
	}

	public Value getValue();
}
