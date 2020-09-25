package com.g2forge.reassert.express.eval.value;

import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;

public interface IValueSystem<Value> {
	public ITextualRenderer<? super Value> getRenderer();

	/**
	 * Test if the two values are equal.
	 * 
	 * @param left
	 * @param right
	 * @return
	 */
	public boolean isEqual(Value left, Value right);

	public IValidation isValid(Value value);
}
