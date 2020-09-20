package com.g2forge.reassert.express.v2.eval;

import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.express.v2.model.IExpression;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

public abstract class AEvaluator<Name, Value, Output, Context extends AEvaluator.IContext<Name, Value, Output>> implements IEvaluator<Name, Value, Output> {
	@Getter
	@RequiredArgsConstructor
	public static class BasicContext<Name, Value, Output> implements IContext<Name, Value, Output> {
		protected final IFunction2<IExpression<Name, Value>, BasicContext<Name, Value, Output>, Output> evaluator;

		@Override
		public Output eval(IExpression<Name, Value> expression) {
			return getEvaluator().apply(expression, this);
		}
	}

	protected interface IContext<Name, Value, Output> {
		public Output eval(IExpression<Name, Value> expression);
	}

	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final IFunction2<IExpression<Name, Value>, Context, Output> evaluator = createEvaluator();

	protected abstract Context createContext();

	protected abstract IFunction2<IExpression<Name, Value>, Context, Output> createEvaluator();

	@Override
	public Output eval(IExpression<Name, Value> expression) {
		return createContext().eval(expression);
	}
}
