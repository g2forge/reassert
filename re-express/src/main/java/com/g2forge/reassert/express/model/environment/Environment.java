package com.g2forge.reassert.express.model.environment;

import java.util.Map;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.variable.IVariable;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Environment<Name, Value> implements IEnvironment<Name, Value> {
	public static class EnvironmentBuilder<Name, Value> implements IEnvironment.IEnvironmentBuilder<Name, Value> {}

	@Singular("bind")
	protected final Map<IVariable<Name, Value>, IExpression<Name, Value>> bindings;

	@Override
	public IOptional<? extends IExpression<Name, Value>> lookup(IVariable<Name, Value> variable) {
		final Map<IVariable<Name, Value>, IExpression<Name, Value>> bindings = getBindings();
		if (!bindings.containsKey(variable)) return NullableOptional.empty();
		return NullableOptional.of(bindings.get(variable));
	}

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
}
