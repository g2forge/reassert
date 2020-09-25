package com.g2forge.reassert.express.eval.operation;

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
	 * Get a function to summarize the result of {@link #combine(Object, Object)} to produce the final result.
	 * 
	 * @return A summary function, or {@code null} if none is necessary.
	 */
	public IFunction1<? super Value, ? extends Value> getSummarizer();

	/**
	 * Get the descriptor for the argument at {@code index}.
	 * 
	 * @param index The {@code index} of the argument to get the descriptor for. An {@code index} of {@code -1} will return the descriptor used for all
	 *            arguments, if and only if all arguments use the same descriptor.
	 * @return the descriptor for the argument at the specified {@code index}.
	 * @throws IllegalArgumentException if {@code index} is less than -1 or {@code index} is greater than the maximum number of allowed arguments.
	 */
	public IArgumentDescriptor<Value> getArgument(int index);

	public IValidation validate(IOperation<?, Value> operation);
}