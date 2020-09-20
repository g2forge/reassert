package com.g2forge.reassert.express.v2.eval.value;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;

public interface IValueSystem<Value> {
	public ITextualRenderer<? super Value> getRenderer();

	/**
	 * Test if the two values are strictly equal. See {@link #isSame(Object, Object)} for mere sameness.
	 * 
	 * @param left
	 * @param right
	 * @return
	 */
	public boolean isEqual(Value left, Value right);

	/**
	 * Test if the two values are the same.
	 * 
	 * @param left
	 * @param right
	 * @return
	 */
	public boolean isSame(Value left, Value right);

	@Note(type = NoteType.TODO, value = "Validation should include explanations")
	public boolean isValid(Value value);
}
