package com.g2forge.reassert.express.v2.model.variable;

import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.alexandria.java.validate.ValidValidation;
import com.g2forge.reassert.express.v2.model.IExpression;

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
	public boolean isSame(IExpression<?, ?> that) {
		if (this == that) return true;
		if ((that == null) || !(that instanceof IClosure)) return false;

		final IClosure<?, ?> cast = (IClosure<?, ?>) that;
		if (!getExpression().isSame(cast.getExpression())) return false;
		if (!getEnvironment().isSame(cast.getEnvironment())) return false;
		return true;
	}

	@Override
	public IValidation validate() {
		return ValidValidation.create();
	}
}
