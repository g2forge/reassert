package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.fluent.optional.IOptional;

public interface IArgumentDescriptor<Value> {
	/**
	 * The identity element of this argument, if there is one.
	 * 
	 * @return The identity element of this argument.
	 */
	public IOptional<? extends Value> getIdentity();

	/**
	 * Get the value, such that when this argument matches the value, the result of the operation is {@link #getZeroOutput()}.
	 * 
	 * @return The zero (input) element of this argument.
	 */
	public IOptional<? extends Value> getZeroInput();

	/**
	 * Get the result of the operation when the argument input is equal to {@link #getZeroInput()}.
	 * 
	 * @return The zero (output) element of this argument.
	 */
	public default IOptional<? extends Value> getZeroOutput() {
		return getZeroInput();
	}
}