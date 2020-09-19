package com.g2forge.reassert.express.v2.eval;

import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.express.v2.model.IExpression;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

public abstract class AEvaluator<Name, Value, Output> implements IEvaluator<Name, Value, Output> {
	@Getter
	@RequiredArgsConstructor
	protected static class Context<Name, Value, Output> {
		protected final IEvaluator<Name, Value, Output> evaluator;
	}

	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final IFunction2<IExpression<Name, Value>, Context<Name, Value, Output>, Output> evaluator = createEvaluator();

	protected abstract IFunction2<IExpression<Name, Value>, Context<Name, Value, Output>, Output> createEvaluator();

	@Override
	public Output eval(IExpression<Name, Value> expression) {
		return getEvaluator().apply(expression, new Context<>(this));
	}
}
