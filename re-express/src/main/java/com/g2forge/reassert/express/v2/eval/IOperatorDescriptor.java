package com.g2forge.reassert.express.v2.eval;

import java.util.function.BinaryOperator;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.express.v2.model.IOperation;

public interface IOperatorDescriptor<Value> {
	/**
	 * The function to combine argument values.
	 * 
	 * @return The function to combine argument values.
	 */
	public BinaryOperator<Value> getCombiner();

	/**
	 * The identity element of this operator.
	 * 
	 * @return The identity element of this operator.
	 */
	public IOptional<Value> getIdentity();

	/**
	 * The function to summarize the result of the {@link #getCombiner()} and generate the final result.
	 * 
	 * @return The function to summarize the result.
	 */
	public IFunction1<Value, Value> getSummarizer();

	/**
	 * The zero element of this operator. {@code null} indicates this operator has no zero element.
	 * 
	 * @return The zero element of this operator.
	 */
	public IOptional<Value> getZero();

	@Note(type = NoteType.TODO, value = "Validation should include explanations")
	public boolean isValid(IOperation operation);
}