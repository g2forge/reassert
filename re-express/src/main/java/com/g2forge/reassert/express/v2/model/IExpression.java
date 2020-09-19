package com.g2forge.reassert.express.v2.model;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;

public interface IExpression<Name, Value> {
	@Note(type = NoteType.TODO, value = "Implement isSame using expressions")
	public boolean isSame(IExpression<?, ?> that);
}
