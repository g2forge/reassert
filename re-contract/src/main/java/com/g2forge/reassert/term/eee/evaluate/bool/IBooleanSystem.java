package com.g2forge.reassert.term.eee.evaluate.bool;

public interface IBooleanSystem<T> {
	public IBooleanOperatorDescriptor<T> getDescriptor(BooleanOperator operator);

	public boolean isValid(T value);
}
