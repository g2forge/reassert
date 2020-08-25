package com.g2forge.reassert.expression.evaluate.bool;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.expression.explain.model.IExplained;
import com.g2forge.reassert.expression.explain.model.IdentityExplainedOperation;
import com.g2forge.reassert.expression.explain.model.ZeroExplainedOperation;
import com.g2forge.reassert.expression.express.IConstant;
import com.g2forge.reassert.expression.express.IExpression;
import com.g2forge.reassert.expression.express.Operation;

public class ExplainedBooleanEvaluator<T> extends ABooleanEvaluator<T, IExplained<T>> {
	public ExplainedBooleanEvaluator(IBooleanSystem<T> system) {
		super(system);
	}

	protected IFunction2<IExpression<T>, Context<T, IExplained<T>>, IExplained<T>> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<T>, Context<T, IExplained<T>>, IExplained<T>> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(IConstant.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IConstant<T> expression = (IConstant<T>) e;
			if (expression.getValue() == null) throw new NullPointerException();
			return expression;
		});
		builder.add(Operation.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final Operation<T> expression = (Operation<T>) e;
			@SuppressWarnings("unchecked")
			final Context<T, IExplained<T>> context = (Context<T, IExplained<T>>) c;

			// Get the descriptor & validate this expression a bit
			final IBooleanOperatorDescriptor<T> descriptor = getDescriptor(expression.getOperator());
			if (!descriptor.isValid(expression.getArguments().size())) throw new IllegalArgumentException();

			// If there are zeros, then explain the result that way
			final List<IExplained<T>> arguments = expression.getArguments().stream().map(context.getEvaluator()::eval).collect(Collectors.toList());
			if (descriptor.getZero() != null) {
				final boolean hasZero = arguments.stream().map(IExplained::get).filter(x -> Objects.equals(x, descriptor.getZero())).findAny().isPresent();
				if (hasZero) return new ZeroExplainedOperation<>(descriptor.getName(), descriptor.getZero(), arguments.stream().map(o -> new ZeroExplainedOperation.Argument(Objects.equals(o.get(), descriptor.getZero()), o)).collect(Collectors.toList()));
			}

			// If there are no zeros, then explain the result in terms of identity values
			final T combined;
			if (descriptor.getCombiner() == null) {
				if (expression.getArguments().size() != 1) throw new IllegalArgumentException();
				combined = HCollection.getOne(arguments).get();
			} else {
				final Stream<T> stream = arguments.stream().map(IExplained::get);
				combined = descriptor.getIdentity() == null ? stream.reduce(descriptor.getCombiner()).get() : stream.reduce(descriptor.getIdentity(), descriptor.getCombiner());
			}
			final T value = descriptor.getSummarizer().apply(combined);
			return new IdentityExplainedOperation<>(value, descriptor.getName(), descriptor.getIdentity(), arguments.stream().map(a -> new IdentityExplainedOperation.Argument(!Objects.equals(a.get(), descriptor.getIdentity()), a)).collect(Collectors.toList()));
		});
		return builder.build();
	}
}
