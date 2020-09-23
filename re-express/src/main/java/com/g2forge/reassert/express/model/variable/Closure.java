package com.g2forge.reassert.express.model.variable;

import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.environment.IEnvironment;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Closure<Name, Value> implements IClosure<Name, Value> {
	protected final IEnvironment<Name, Value> environment;

	protected final IExpression<Name, Value> expression;

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
}
