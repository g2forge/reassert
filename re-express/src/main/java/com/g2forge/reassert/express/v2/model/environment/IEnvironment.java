package com.g2forge.reassert.express.v2.model.environment;

import java.util.Map;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.builder.IValidatingBuilder;
import com.g2forge.alexandria.java.validate.IValidatable;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.variable.IVariable;

public interface IEnvironment<Name, Value> extends IValidatable {
	public interface IEnvironmentBuilder<Name, Value> extends IValidatingBuilder<IEnvironment<Name, Value>> {
		public IEnvironmentBuilder<Name, Value> bind(IVariable<Name, Value> variable, IExpression<Name, Value> expression);

		public default IEnvironmentBuilder<Name, Value> bind$(IVariable<Name, Value> variable, Value value) {
			return bind(variable, new Literal<>(value));
		}
	}

	public Map<IVariable<Name, Value>, IExpression<Name, Value>> getBindings();

	public IOptional<? extends IExpression<Name, Value>> lookup(IVariable<Name, Value> variable);

	public default IEnvironment<Name, Value> override(IEnvironment<Name, Value> override) {
		return new OverrideEnvironment<>(override, this);
	}

	public default boolean isSame(IEnvironment<?, ?> that) {
		if (this == that) return true;
		if ((that == null) || !(that instanceof IEnvironment)) return false;

		@SuppressWarnings("unchecked")
		final IEnvironment<Name, Value> cast = (IEnvironment<Name, Value>) that;

		if (!getBindings().keySet().equals(cast.getBindings().keySet())) return false;
		for (IVariable<Name, Value> variable : getBindings().keySet()) {
			if (!lookup(variable).get().isSame(cast.lookup(variable).get())) return false;
		}
		return true;
	}
}
