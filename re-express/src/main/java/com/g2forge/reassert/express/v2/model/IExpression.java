package com.g2forge.reassert.express.v2.model;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.validate.IValidatable;

public interface IExpression<Name, Value> extends IValidatable {
	@Note(type = NoteType.TODO, value = "Implement isSame using expressions")
	public boolean isSame(IExpression<?, ?> that);
}
