package com.g2forge.reassert.express.v2.eval;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.error.UnreachableCodeError;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.ILiteral;
import com.g2forge.reassert.express.v2.model.environment.IEnvironment;
import com.g2forge.reassert.express.v2.model.operation.ExplainedOperation;
import com.g2forge.reassert.express.v2.model.operation.IExplainedOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.operation.ZeroExplainedOperation;
import com.g2forge.reassert.express.v2.model.variable.ExplainedClosure;
import com.g2forge.reassert.express.v2.model.variable.ExplainedVariable;
import com.g2forge.reassert.express.v2.model.variable.IClosure;
import com.g2forge.reassert.express.v2.model.variable.IExplainedClosure;
import com.g2forge.reassert.express.v2.model.variable.IVariable;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class ExplainingEvaluator<Name, Value> extends AEvaluator<Name, Value, IExplained<Value>, AEvaluator.BasicContext<Name, Value, IExplained<Value>>> {
	@Getter
	@RequiredArgsConstructor
	protected static class RecordingEnvironment<Name, Value> implements IEnvironment<Name, Value> {
		protected class LookupResult extends NullableOptional<IExpression<Name, Value>> {
			protected final IVariable<Name, Value> variable;

			public LookupResult(IVariable<Name, Value> variable) {
				super();
				this.variable = variable;
			}

			public LookupResult(IVariable<Name, Value> variable, IExpression<Name, Value> value) {
				super(value);
				this.variable = variable;
			}

			public void record(IOptional<IExplained<Value>> explained) {
				getRecord().put(variable, new IExplainedClosure.Binding<>(IExplained.Relevance.Dominant, variable, explained));
			}
		}

		protected final IEnvironment<Name, Value> environment;

		protected final Map<IVariable<Name, Value>, IExplainedClosure.Binding<Name, Value>> record = new LinkedHashMap<>();

		@Override
		public Map<IVariable<Name, Value>, IExpression<Name, Value>> getBindings() {
			return getEnvironment().getBindings();
		}

		@Override
		public IOptional<? extends IExpression<Name, Value>> lookup(IVariable<Name, Value> variable) {
			final IOptional<? extends IExpression<Name, Value>> retVal = getEnvironment().lookup(variable);
			return retVal.isEmpty() ? new LookupResult(variable) : new LookupResult(variable, retVal.get());
		}

		@Override
		public IValidation validate() {
			return getEnvironment().validate();
		}
	}

	protected final IValueSystem<Value> valueSystem;

	protected final IOperationSystem<Value> operationSystem;

	@Override
	protected BasicContext<Name, Value, IExplained<Value>> createContext() {
		return new AEvaluator.BasicContext<>(getEvaluator());
	}

	@Override
	protected IFunction2<IExpression<Name, Value>, AEvaluator.BasicContext<Name, Value, IExplained<Value>>, IExplained<Value>> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<Name, Value>, AEvaluator.BasicContext<Name, Value, IExplained<Value>>, IExplained<Value>> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(ILiteral.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final ILiteral<Name, Value> expression = (ILiteral<Name, Value>) e;
			return expression;
		});
		builder.add(IOperation.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IOperation<Name, Value> expression = (IOperation<Name, Value>) e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, IExplained<Value>> context = (AEvaluator.BasicContext<Name, Value, IExplained<Value>>) c;

			final IOperatorDescriptor<Value> descriptor = getOperationSystem().getDescriptor(expression.getOperator());
			descriptor.validate(expression).throwIfInvalid();

			final IValueSystem<Value> valueSystem = getValueSystem();
			final IOptional<? extends Value> zero = descriptor.getZero();
			final IOptional<? extends Value> identity = descriptor.getIdentity();

			boolean hasZero = false;
			final List<IExplainedOperation.Argument<Value>> evaluated = new ArrayList<>();
			for (IExpression<Name, Value> argument : expression.getArguments()) {
				final IExplained<Value> result;
				try {
					result = context.eval(argument);
				} catch (Throwable throwable) {
					throw new RuntimeException(String.format("Failed to evaluate %1$s!", expression.getOperator()), throwable);
				}

				final IExplained.Relevance relevance;
				if (!zero.isEmpty() && valueSystem.isSame(result.get(), zero.get())) {
					hasZero = true;
					relevance = IExplained.Relevance.Dominant;
				} else if (hasZero) relevance = IExplained.Relevance.Unevaluated;
				else if (!identity.isEmpty() && valueSystem.isSame(result.get(), identity.get())) relevance = IExplained.Relevance.Identity;
				else relevance = IExplained.Relevance.Combined;

				evaluated.add(new IExplainedOperation.Argument<>(relevance, result));
			}

			if (hasZero) return new ZeroExplainedOperation<Value>(expression.getOperator(), zero.get(), evaluated);

			final Stream<Value> stream = evaluated.stream().map(IExplainedOperation.Argument::getExplained).map(IExplained::get);
			final Value reduced;
			if (identity.isEmpty()) reduced = stream.reduce(descriptor::combine).get();
			else reduced = stream.reduce(identity.get(), descriptor::combine);

			final IFunction1<? super Value, ? extends Value> summarizer = descriptor.getSummarizer();
			final Value result = summarizer == null ? reduced : summarizer.apply(reduced);
			return new ExplainedOperation<>(expression.getOperator(), result, identity, evaluated);
		});
		builder.add(IClosure.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IClosure<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, IExplained<Value>> context = (AEvaluator.BasicContext<Name, Value, IExplained<Value>>) c;

			final IExplained<Value> retVal;
			final RecordingEnvironment<Name, Value> environment = new RecordingEnvironment<>(expression.getEnvironment());
			try (ICloseable env = context.environment(environment)) {
				retVal = context.eval(expression.getExpression());
			}

			return new ExplainedClosure<>(retVal, new ArrayList<>(environment.getRecord().values()));
		});
		builder.add(IVariable.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IVariable<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, IExplained<Value>> context = (AEvaluator.BasicContext<Name, Value, IExplained<Value>>) c;

			final IOptional<? extends IExpression<Name, Value>> lookup = context.getEnvironment().lookup(expression);
			final IOptional<IExplained<Value>> explained = lookup.map(context::eval);

			if (lookup instanceof RecordingEnvironment.LookupResult) {
				@SuppressWarnings("unchecked")
				final RecordingEnvironment<Name, Value>.LookupResult cast = (RecordingEnvironment<Name, Value>.LookupResult) lookup;
				cast.record(explained);
			} else if (!lookup.isEmpty()) throw new UnreachableCodeError(String.format("Somehow a variable was bound in a non-reording environment, this is a bug in %1$s", getClass()));

			return new ExplainedVariable<>(expression, explained);
		});
		return builder.build();
	}
}
