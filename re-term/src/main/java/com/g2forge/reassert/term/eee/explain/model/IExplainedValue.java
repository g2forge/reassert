package com.g2forge.reassert.term.eee.explain.model;

public interface IExplainedValue<T> extends IExplained<T> {
	@Override
	public default T get() {
		return getValue();
	}
	
	public T getValue();
}
