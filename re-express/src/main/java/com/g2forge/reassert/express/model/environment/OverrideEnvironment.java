package com.g2forge.reassert.express.model.environment;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.validate.CompositeValidation;
import com.g2forge.alexandria.java.validate.IValidatable;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.variable.IVariable;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Singular;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class OverrideEnvironment<Name, Value> implements IEnvironment<Name, Value> {
	@Singular
	protected final List<IEnvironment<Name, Value>> environments;

	@Getter(lazy = true)
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	private final Map<IVariable<Name, Value>, IExpression<Name, Value>> bindings = computeBindings();

	@SafeVarargs
	public OverrideEnvironment(IEnvironment<Name, Value>... environments) {
		this(HCollection.asList(environments));
	}

	protected Map<IVariable<Name, Value>, IExpression<Name, Value>> computeBindings() {
		final Map<IVariable<Name, Value>, IExpression<Name, Value>> retVal = new LinkedHashMap<>();
		for (IEnvironment<Name, Value> environment : getEnvironments()) {
			retVal.putAll(environment.getBindings());
		}
		return Collections.unmodifiableMap(retVal);
	}

	@Override
	public IOptional<? extends IExpression<Name, Value>> lookup(IVariable<Name, Value> variable) {
		for (IEnvironment<Name, Value> environment : getEnvironments()) {
			final IOptional<? extends IExpression<Name, Value>> result = environment.lookup(variable);
			if (!result.isEmpty()) return result;
		}
		return NullableOptional.empty();
	}

	@Override
	public IValidation validate() {
		return CompositeValidation.create(getEnvironments().stream().map(IValidatable::validate).collect(Collectors.toList()));
	}
}