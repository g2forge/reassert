package com.g2forge.reassert.express.model.constant;

import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;

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
	public IValidation validate() {
		return ValidValidation.create();
	}
}
