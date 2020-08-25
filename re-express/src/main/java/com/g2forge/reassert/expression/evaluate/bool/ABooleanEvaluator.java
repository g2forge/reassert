package com.g2forge.reassert.expression.evaluate.bool;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.expression.evaluate.IEvaluator;
import com.g2forge.reassert.expression.express.IExpression;
import com.g2forge.reassert.expression.express.Operation;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public abstract class ABooleanEvaluator<T, R> implements IEvaluator<T, R> {
	@Getter
	@RequiredArgsConstructor
	protected static class Context<T, R> {
		protected final IEvaluator<T, R> evaluator;
	}

	protected final IBooleanSystem<T> system;

	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final IFunction2<IExpression<T>, Context<T, R>, R> evaluator = createEvaluator();

	protected abstract IFunction2<IExpression<T>, Context<T, R>, R> createEvaluator();

	@Override
	public R eval(IExpression<T> expression) {
		return getEvaluator().apply(expression, new Context<>(this));
	}

	protected IBooleanOperatorDescriptor<T> getDescriptor(Operation.Operator operator) {
		final BooleanOperator retVal;
		switch (operator) {
			case AND:
				retVal = BooleanOperator.AND;
				break;
			case OR:
				retVal = BooleanOperator.OR;
				break;
			case NOT:
				retVal = BooleanOperator.NOT;
				break;
			default:
				throw new EnumException(Operation.Operator.class, operator);
		}
		return getSystem().getDescriptor(retVal);
	}
}
