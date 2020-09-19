package com.g2forge.reassert.express.v2.eval.bool;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.adt.tuple.ITuple1G_;
import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.express.v2.eval.AEvaluator;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.ILiteral;
import com.g2forge.reassert.express.v2.model.operation.IOperation;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class BooleanEvaluator<Name> extends AEvaluator<Name, Boolean, Boolean> {
	protected static class OrThrowable<T> implements ITuple1G_<T> {
		protected final T value;

		@Getter
		protected final Throwable throwable;

		public OrThrowable(T value) {
			this.value = value;
			this.throwable = null;
		}

		public OrThrowable(Throwable throwable) {
			if (throwable == null) throw new NullPointerException();
			this.value = null;
			this.throwable = throwable;
		}

		@Override
		public T get0() {
			final Throwable throwable = getThrowable();
			if (throwable != null) HError.throwQuietly(throwable);
			return value;
		}
	}

	protected final IValueSystem<Boolean> valueSystem;

	protected final IOperationSystem<Boolean> operationSystem;

	@Override
	protected IFunction2<IExpression<Name, Boolean>, Context<Name, Boolean, Boolean>, Boolean> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<Name, Boolean>, Context<Name, Boolean, Boolean>, Boolean> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(ILiteral.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final ILiteral<Name, Boolean> expression = (ILiteral<Name, Boolean>) e;
			final Boolean value = expression.get();
			if (!getValueSystem().isValid(value)) throw new NullPointerException();
			return value;
		});
		builder.add(IOperation.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IOperation<Name, Boolean> expression = (IOperation<Name, Boolean>) e;
			@SuppressWarnings("unchecked")
			final Context<Name, Boolean, Boolean> context = (Context<Name, Boolean, Boolean>) c;

			final IOperatorDescriptor<Boolean> descriptor = getOperationSystem().getDescriptor(expression.getOperator());
			if (!descriptor.isValid(expression)) throw new IllegalArgumentException();

			final IValueSystem<Boolean> valueSystem = getValueSystem();
			final IOptional<? extends Boolean> zero = descriptor.getZero();
			final IOptional<? extends Boolean> identity = descriptor.getIdentity();

			final List<OrThrowable<Boolean>> evaluated = new ArrayList<>();
			for (IExpression<Name, Boolean> argument : expression.getArguments()) {
				final Boolean result;
				try {
					result = context.getEvaluator().eval(argument);
				} catch (Throwable throwable) {
					evaluated.add(new OrThrowable<>(throwable));
					continue;
				}

				if (!zero.isEmpty() && valueSystem.isSame(result, zero.get())) return zero.get();
				if (identity.isEmpty() || !valueSystem.isSame(result, identity.get())) evaluated.add(new OrThrowable<>(result));
			}

			final List<Throwable> throwables = evaluated.stream().map(OrThrowable::getThrowable).filter(Objects::nonNull).collect(Collectors.toList());
			if (!throwables.isEmpty()) throw HError.multithrow(String.format("Failed to evaluate %1$s!", expression.getOperator()), throwables);

			final Stream<Boolean> stream = evaluated.stream().map(ITuple1G_::get0);
			final Boolean reduced;
			if (identity.isEmpty()) reduced = stream.reduce(descriptor::combine).get();
			else reduced = stream.reduce(identity.get(), descriptor::combine);

			final IFunction1<? super Boolean, ? extends Boolean> summarizer = descriptor.getSummarizer();
			return summarizer == null ? reduced : summarizer.apply(reduced);
		});
		return builder.build();
	}
}
