package com.g2forge.reassert.express.v2.eval;

import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.IConstant;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.environment.EmptyEnvironment;
import com.g2forge.reassert.express.v2.model.environment.IEnvironment;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.variable.IClosure;
import com.g2forge.reassert.express.v2.model.variable.IVariable;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReductionRewriter<Name, Value> extends AEvaluator<Name, Value, IExpression<Name, Value>, ReductionRewriter.Context<Name, Value>> implements IRewriter<Name, Value> {
	@Getter(AccessLevel.PROTECTED)
	public static class Context<Name, Value> implements AEvaluator.IContext<Name, Value, IExpression<Name, Value>> {
		protected final IFunction2<IExpression<Name, Value>, Context<Name, Value>, IExpression<Name, Value>> evaluator;

		protected final Stack<IEnvironment<Name, Value>> stack = new Stack<>();

		public Context(IFunction2<IExpression<Name, Value>, Context<Name, Value>, IExpression<Name, Value>> evaluator) {
			this.evaluator = evaluator;
			this.stack.push(new EmptyEnvironment<>());
		}

		public ICloseable environment(IEnvironment<Name, Value> environment) {
			final IEnvironment<Name, Value> top = this.stack.peek().override(environment);
			this.stack.push(top);
			return () -> {
				if (this.stack.peek() != top) throw new IllegalStateException();
				this.stack.pop();
			};
		}

		@Override
		public IExpression<Name, Value> eval(IExpression<Name, Value> expression) {
			return getEvaluator().apply(expression, this);
		}

		public IExpression<Name, Value> eval(IExpression<Name, Value> original, IExpression<Name, Value> expression) {
			if (original == expression) return expression;
			return eval(expression);
		}

		public IEnvironment<Name, Value> getEnvironment() {
			return stack.peek();
		}
	}

	public enum Reduction {
		TrivialOperations,
		ApplyClosures;
	}

	protected final IOperationSystem<Value> operationSystem;

	protected final Set<Reduction> reductions;

	public ReductionRewriter(IOperationSystem<Value> operationSystem) {
		this(operationSystem, EnumSet.allOf(Reduction.class));
	}

	public ReductionRewriter(IOperationSystem<Value> operationSystem, Reduction... reductions) {
		this(operationSystem, HCollection.asSet(Reduction.class, reductions));
	}

	@Override
	protected Context<Name, Value> createContext() {
		return new Context<>(getEvaluator());
	}

	@Override
	protected IFunction2<IExpression<Name, Value>, Context<Name, Value>, IExpression<Name, Value>> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<Name, Value>, Context<Name, Value>, IExpression<Name, Value>> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(IConstant.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IConstant<Name, Value> expression = e;
			return expression;
		});
		builder.add(IOperation.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IOperation<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final Context<Name, Value> context = c;

			final List<IExpression<Name, Value>> arguments = expression.getArguments().stream().map(context::eval).collect(Collectors.toList());
			boolean same = true;
			for (int i = 0; i < arguments.size(); i++) {
				if (arguments.get(i) != expression.getArguments().get(i)) {
					same = false;
					break;
				}
			}

			if (getReductions().contains(Reduction.TrivialOperations)) {
				if (arguments.size() <= 1) {
					final IOperatorDescriptor<Value> descriptor = getOperationSystem().getDescriptor(expression.getOperator());
					if (arguments.isEmpty() && !descriptor.getIdentity().isEmpty()) return context.eval(expression, new Literal<>(descriptor.getIdentity().get()));
					if (descriptor.getSummarizer() == null) return context.eval(expression, HCollection.getOne(arguments));
				}
			}

			return context.eval(expression, same ? expression : expression.getOperator().<Name, Value>builder().arguments(arguments).valid());
		});
		builder.add(IClosure.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IClosure<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final Context<Name, Value> context = c;

			if (getReductions().contains(Reduction.ApplyClosures)) {
				try (ICloseable env = context.environment(expression.getEnvironment())) {
					final IExpression<Name, Value> reduced = context.eval(expression.getExpression());
					if (reduced != expression.getExpression()) return context.eval(expression, reduced);
				}
			}

			return expression;
		});
		builder.add(IVariable.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IVariable<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final Context<Name, Value> context = c;

			if (getReductions().contains(Reduction.ApplyClosures)) {
				final IOptional<? extends IExpression<Name, Value>> result = context.getEnvironment().lookup(expression);
				if (!result.isEmpty()) return context.eval(expression, result.get());
			}

			return expression;
		});
		return builder.build();
	}
}
