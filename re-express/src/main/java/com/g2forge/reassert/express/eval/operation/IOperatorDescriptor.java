package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.model.operation.IOperation;

public interface IOperatorDescriptor<Value> {
	/**
	 * Combine two arguments.
	 * 
	 * @param left The left argument to be combined.
	 * @param right The right argument to be combined.
	 * @return The result of the combination.
	 */
	public Value combine(Value left, Value right);

	/**
	 * The identity element of this operator.
	 * 
	 * @return The identity element of this operator.
	 */
	public IOptional<? extends Value> getIdentity();

	/**
	 * Get a function to summarize the result of {@link #combine(Object, Object)} to produce the final result.
	 * 
	 * @return A summary function, or {@code null} if none is necessary.
	 */
	public IFunction1<? super Value, ? extends Value> getSummarizer();

	/**
	 * The zero element of this operator. {@code null} indicates this operator has no zero element.
	 * 
	 * @return The zero element of this operator.
	 */
	public IOptional<? extends Value> getZero();

	public IValidation validate(IOperation<?, Value> operation);
}