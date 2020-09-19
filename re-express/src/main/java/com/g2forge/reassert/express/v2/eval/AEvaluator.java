package com.g2forge.reassert.express.v2.eval;

import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.express.v2.model.IExpression;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

public abstract class AEvaluator<Name, Value> implements IEvaluator<Name, Value, Value> {
	@Getter
	@RequiredArgsConstructor
	protected static class Context<Name, Value> {
		protected final IEvaluator<Name, Value, Value> evaluator;
	}

	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final IFunction2<IExpression<Name, Value>, Context<Name, Value>, Value> evaluator = createEvaluator();
	
	protected abstract IFunction2<IExpression<Name, Value>, Context<Name, Value>, Value> createEvaluator();

	@Override
	public Value eval(IExpression<Name, Value> expression) {
		return getEvaluator().apply(expression, new Context<>(this));
	}
}
