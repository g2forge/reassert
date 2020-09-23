package com.g2forge.reassert.express.eval;

import java.util.Stack;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.environment.EmptyEnvironment;
import com.g2forge.reassert.express.model.environment.IEnvironment;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class AEvaluator<Name, Value, Output, Context extends AEvaluator.IContext<Name, Value, Output>> implements IEvaluator<Name, Value, Output> {
	@Getter(AccessLevel.PROTECTED)
	protected static abstract class AContext<Name, Value, Output, Context extends AEvaluator.IContext<Name, Value, Output>> implements AEvaluator.IContext<Name, Value, Output> {
		protected final IFunction2<IExpression<Name, Value>, Context, Output> evaluator;

		protected final Stack<IEnvironment<Name, Value>> stack;

		public AContext(IFunction2<IExpression<Name, Value>, Context, Output> evaluator) {
			this.evaluator = evaluator;
			this.stack = new Stack<>();
			this.stack.push(new EmptyEnvironment<>());
		}

		public ICloseable environment(IEnvironment<Name, Value> environment) {
			final Stack<IEnvironment<Name, Value>> stack1 = getStack();
			final IEnvironment<Name, Value> top = stack1.peek().override(environment);
			stack1.push(top);
			return () -> {
				final Stack<IEnvironment<Name, Value>> stack2 = getStack();
				if (stack2.peek() != top) throw new IllegalStateException();
				stack2.pop();
			};
		}

		@Override
		public Output eval(IExpression<Name, Value> expression) {
			return getEvaluator().apply(expression, getThis());
		}

		public IEnvironment<Name, Value> getEnvironment() {
			return getStack().peek();
		}

		public abstract Context getThis();
	}

	public static class BasicContext<Name, Value, Output> extends AEvaluator.AContext<Name, Value, Output, BasicContext<Name, Value, Output>> {
		public BasicContext(IFunction2<IExpression<Name, Value>, BasicContext<Name, Value, Output>, Output> evaluator) {
			super(evaluator);
		}

		@Override
		public BasicContext<Name, Value, Output> getThis() {
			return this;
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
