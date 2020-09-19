package com.g2forge.reassert.express.evaluate.bool;

import java.util.function.BinaryOperator;

import com.g2forge.alexandria.java.adt.name.IStringNamed;
import com.g2forge.alexandria.java.function.IFunction1;

public interface IBooleanOperatorDescriptor<T> extends IStringNamed {
	public BinaryOperator<T> getCombiner();

	/**
	 * The identity element of this operator. {@code null} indicates this operator has no identity element.
	 * 
	 * @return The identity element of this operator.
	 */
	public T getIdentity();

	public IFunction1<T, T> getSummarizer();

	/**
	 * The zero element of this operator. {@code null} indicates this operator has no zero element.
	 * 
	 * @return The zero element of this operator.
	 */
	public T getZero();

	public boolean isValid(int nArguments);
}