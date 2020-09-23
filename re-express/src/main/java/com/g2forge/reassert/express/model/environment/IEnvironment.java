package com.g2forge.reassert.express.model.environment;

import java.util.Map;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.builder.IValidatingBuilder;
import com.g2forge.alexandria.java.validate.IValidatable;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.variable.IVariable;
import com.g2forge.reassert.express.model.variable.Variable;

public interface IEnvironment<Name, Value> extends IValidatable {
	public interface IEnvironmentBuilder<Name, Value> extends IValidatingBuilder<IEnvironment<Name, Value>> {
		public IEnvironmentBuilder<Name, Value> bind(IVariable<Name, Value> variable, IExpression<Name, Value> expression);

		public default IEnvironmentBuilder<Name, Value> bind$(IVariable<Name, Value> variable, Value value) {
			return bind(variable, new Literal<>(value));
		}
		
		public default IEnvironmentBuilder<Name, Value> bind$(Name name, Value value) {
			return bind$(new Variable<>(name), value);
		}
	}

	public Map<IVariable<Name, Value>, IExpression<Name, Value>> getBindings();

	public IOptional<? extends IExpression<Name, Value>> lookup(IVariable<Name, Value> variable);

	public default IEnvironment<Name, Value> override(IEnvironment<Name, Value> override) {
		return new OverrideEnvironment<>(override, this);
	}
}
