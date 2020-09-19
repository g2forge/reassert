package com.g2forge.reassert.express.evaluate.bool;

import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.express.express.IConstant;
import com.g2forge.reassert.express.express.IExpression;
import com.g2forge.reassert.express.express.Operation;

public class BooleanEvaluator<T> extends ABooleanEvaluator<T, T> {
	public BooleanEvaluator(IBooleanSystem<T> system) {
		super(system);
	}

	@Override
	protected IFunction2<IExpression<T>, Context<T, T>, T> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<T>, Context<T, T>, T> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(IConstant.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IConstant<T> expression = (IConstant<T>) e;
			final T value = expression.getValue();
			if (!getSystem().isValid(value)) throw new NullPointerException();
			return value;
		});
		builder.add(Operation.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final Operation<T> expression = (Operation<T>) e;
			@SuppressWarnings("unchecked")
			final Context<T, T> context = (Context<T, T>) c;

			final IBooleanOperatorDescriptor<T> descriptor = getDescriptor(expression.getOperator());
			if (!descriptor.isValid(expression.getArguments().size())) throw new IllegalArgumentException();

			final T combined;
			if (descriptor.getCombiner() == null) {
				if (expression.getArguments().size() != 1) throw new IllegalArgumentException();
				combined = context.getEvaluator().eval(HCollection.getOne(expression.getArguments()));
			} else {
				final Stream<T> stream = expression.getArguments().stream().map(context.getEvaluator()::eval);
				combined = descriptor.getIdentity() == null ? stream.reduce(descriptor.getCombiner()).get() : stream.reduce(descriptor.getIdentity(), descriptor.getCombiner());
			}
			return descriptor.getSummarizer().apply(combined);
		});
		return builder.build();
	}
}
