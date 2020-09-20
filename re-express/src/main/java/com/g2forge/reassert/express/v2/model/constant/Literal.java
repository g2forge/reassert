package com.g2forge.reassert.express.v2.model.constant;

import java.util.Objects;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;
import com.g2forge.reassert.express.v2.model.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Literal<Name, Value> implements ILiteral<Name, Value> {
	protected final Name name;

	protected final Value value;

	public Literal(Value value) {
		this(null, value);
	}

	@Override
	public Value get() {
		return getValue();
	}

	@Note(type = NoteType.TODO, value = "Implement isSame using expressions & value system")
	@Override
	public boolean isSame(IExpression<?, ?> that) {
		if (this == that) return true;
		if ((that == null) || !(that instanceof ILiteral)) return false;

		final ILiteral<?, ?> cast = (ILiteral<?, ?>) that;
		if (!Objects.equals(get(), cast.get())) return false;
		if (!Objects.equals(getName(), cast.getName())) return false;
		return true;
	}

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
}
