package com.g2forge.reassert.express.v2.model.variable;

import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Variable<Name, Value> implements IVariable<Name, Value> {
	protected final Name name;

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
}
