package com.g2forge.reassert.express.model.environment;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.variable.IVariable;

import lombok.EqualsAndHashCode;

@EqualsAndHashCode
public class EmptyEnvironment<Name, Value> implements IEnvironment<Name, Value> {
	@Override
	public IOptional<? extends IExpression<Name, Value>> lookup(IVariable<Name, Value> variable) {
		return NullableOptional.empty();
	}

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
}