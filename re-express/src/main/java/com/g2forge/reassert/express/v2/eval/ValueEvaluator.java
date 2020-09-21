package com.g2forge.reassert.express.v2.eval;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.error.OrThrowable;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.express.v2.eval.error.EvalFailedException;
import com.g2forge.reassert.express.v2.eval.error.VariableUnboundException;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.ILiteral;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.variable.IClosure;
import com.g2forge.reassert.express.v2.model.variable.IVariable;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public class ValueEvaluator<Name, Value> extends AEvaluator<Name, Value, Value, AEvaluator.BasicContext<Name, Value, Value>> {
	protected final IValueSystem<Value> valueSystem;

	protected final IOperationSystem<Value> operationSystem;

	@Override
	protected BasicContext<Name, Value, Value> createContext() {
		return new AEvaluator.BasicContext<>(getEvaluator());
	}

	@Override
	protected IFunction2<IExpression<Name, Value>, AEvaluator.BasicContext<Name, Value, Value>, Value> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<Name, Value>, AEvaluator.BasicContext<Name, Value, Value>, Value> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(ILiteral.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final ILiteral<Name, Value> expression = (ILiteral<Name, Value>) e;
			final Value value = expression.get();
			getValueSystem().isValid(value).throwIfInvalid();
			return value;
		});
		builder.add(IOperation.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IOperation<Name, Value> expression = (IOperation<Name, Value>) e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, Value> context = (AEvaluator.BasicContext<Name, Value, Value>) c;

			final IOperatorDescriptor<Value> descriptor = getOperationSystem().getDescriptor(expression.getOperator());
			descriptor.validate(expression).throwIfInvalid();

			final IValueSystem<Value> valueSystem = getValueSystem();
			final IOptional<? extends Value> zero = descriptor.getZero();
			final IOptional<? extends Value> identity = descriptor.getIdentity();

			final List<OrThrowable<Value>> evaluated = new ArrayList<>();
			for (IExpression<Name, Value> argument : expression.getArguments()) {
				final Value result;
				try {
					result = context.eval(argument);
				} catch (Throwable throwable) {
					evaluated.add(new OrThrowable<>(throwable));
					continue;
				}

				if (!zero.isEmpty() && valueSystem.isSame(result, zero.get())) return zero.get();
				if (identity.isEmpty() || !valueSystem.isSame(result, identity.get())) evaluated.add(new OrThrowable<>(result));
			}

			final List<Value> list = evaluated.stream().collect(HError.collector(() -> new EvalFailedException(expression), false, Collectors.<Value>toList()));

			final Value reduced;
			if (identity.isEmpty()) reduced = list.stream().reduce(descriptor::combine).get();
			else reduced = list.stream().reduce(identity.get(), descriptor::combine);

			final IFunction1<? super Value, ? extends Value> summarizer = descriptor.getSummarizer();
			return summarizer == null ? reduced : summarizer.apply(reduced);
		});
		builder.add(IClosure.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IClosure<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, Value> context = (AEvaluator.BasicContext<Name, Value, Value>) c;

			try (ICloseable env = context.environment(expression.getEnvironment())) {
				return context.eval(expression.getExpression());
			}
		});
		builder.add(IVariable.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IVariable<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, Value> context = (AEvaluator.BasicContext<Name, Value, Value>) c;

			final IOptional<? extends IExpression<Name, Value>> result = context.getEnvironment().lookup(expression);
			if (result.isEmpty()) throw new VariableUnboundException(expression);
			return context.eval(result.get());
		});
		builder.fallback((e, c) -> {
			throw new EvalFailedException(e);
		});
		return builder.build();
	}
}
