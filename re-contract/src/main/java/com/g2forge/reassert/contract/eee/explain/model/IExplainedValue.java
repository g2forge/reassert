package com.g2forge.reassert.contract.eee.explain.model;

public interface IExplainedValue<T> extends IExplained<T> {
	@Override
	public default T get() {
		return getValue();
	}
	
	public T getValue();
}
