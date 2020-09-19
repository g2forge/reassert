package com.g2forge.reassert.express.v2.model.constant;

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

	@Override
	public boolean isSame(IExpression<?, ?> that) {
		if (this == that) return true;
		if ((that == null) || !(that instanceof ILiteral)) return false;

		final ILiteral<?, ?> cast = (ILiteral<?, ?>) that;
		if (!get().equals(cast.get())) return false;
		if (!getName().equals(cast.getName())) return false;
		return true;
	}

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
}
