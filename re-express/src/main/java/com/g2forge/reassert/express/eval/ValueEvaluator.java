package com.g2forge.reassert.express.eval;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.error.OrThrowable;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.express.eval.error.EvalFailedException;
import com.g2forge.reassert.express.eval.error.VariableUnboundException;
import com.g2forge.reassert.express.eval.operation.IArgumentDescriptor;
import com.g2forge.reassert.express.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.eval.value.IValueSystem;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.ILiteral;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.variable.IClosure;
import com.g2forge.reassert.express.model.variable.IVariable;

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

			final IOperatorDescriptor<Value> operatorDescriptor = getOperationSystem().getDescriptor(expression.getOperator());
			operatorDescriptor.validate(expression).throwIfInvalid();

			final IValueSystem<Value> valueSystem = getValueSystem();

			final List<? extends IExpression<Name, Value>> arguments = expression.getArguments();
			final List<OrThrowable<Value>> evaluated = new ArrayList<>();
			final Set<Value> identities = new LinkedHashSet<>();
			for (int i = 0; i < arguments.size(); i++) {
				final IExpression<Name, Value> argument = arguments.get(i);
				final Value result;
				try {
					result = context.eval(argument);
				} catch (Throwable throwable) {
					evaluated.add(new OrThrowable<>(throwable));
					continue;
				}

				final IArgumentDescriptor<Value> argumentDescriptor = operatorDescriptor.getArgument(i);
				final IOptional<? extends Value> zero = argumentDescriptor.getZeroInput();
				final IOptional<? extends Value> identity = argumentDescriptor.getIdentity();
				if (!zero.isEmpty() && valueSystem.isEqual(result, zero.get())) return argumentDescriptor.getZeroOutput().get();
				if (identity.isEmpty() || !valueSystem.isEqual(result, identity.get())) evaluated.add(new OrThrowable<>(result));
				if (identity.isNotEmpty()) identities.add(identity.get());
			}
			if (arguments.isEmpty()) {
				final IOptional<? extends Value> identity = operatorDescriptor.getArgument(-1).getIdentity();
				if (identity.isNotEmpty()) identities.add(identity.get());
			}

			final List<Value> list = evaluated.stream().collect(HError.collector(() -> new EvalFailedException(expression), false, Collectors.<Value>toList()));

			final Value reduced;
			if (identities.size() != 1) reduced = list.stream().reduce(operatorDescriptor::combine).get();
			else reduced = list.stream().reduce(HCollection.getOne(identities), operatorDescriptor::combine);

			final IFunction1<? super Value, ? extends Value> summarizer = operatorDescriptor.getSummarizer();
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
