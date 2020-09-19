package com.g2forge.reassert.express.v2.eval.operation;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.reassert.express.v2.model.operation.IOperation;

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
	 * The zero element of this operator. {@code null} indicates this operator has no zero element.
	 * 
	 * @return The zero element of this operator.
	 */
	public IOptional<? extends Value> getZero();

	@Note(type = NoteType.TODO, value = "Validation should include explanations")
	public boolean isValid(IOperation<?, Value> operation);

	/**
	 * Summarize the result of {@link #combine(Object, Object)} to produce the final result.
	 * 
	 * @param combined The result of combination.
	 * @return The operation result.
	 */
	public Value summarize(Value combined);
}