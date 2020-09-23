package com.g2forge.reassert.express.model.environment;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.variable.IVariable;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class ATypeSwitchEnvironment<Name, Value> implements IEnvironment<Name, Value> {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<? super Name, ? extends IOptional<? extends IExpression<Name, Value>>> function = computeFunction();

	protected IFunction1<? super Name, ? extends IOptional<? extends IExpression<Name, Value>>> computeFunction() {
		final TypeSwitch1.FunctionBuilder<Name, IOptional<? extends IExpression<Name, Value>>> builder = new TypeSwitch1.FunctionBuilder<Name, IOptional<? extends IExpression<Name, Value>>>();
		with(builder);
		builder.fallback(variable -> NullableOptional.empty());
		return builder.build();
	}

	@Override
	public IOptional<? extends IExpression<Name, Value>> lookup(IVariable<Name, Value> variable) {
		return getFunction().apply(variable.getName());
	}

	protected IOptional<Literal<Name, Value>> of(final Value retVal) {
		return NullableOptional.of(new Literal<>(retVal));
	}

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
	
	protected abstract void with(TypeSwitch1.FunctionBuilder<Name, IOptional<? extends IExpression<Name, Value>>> builder);
}
