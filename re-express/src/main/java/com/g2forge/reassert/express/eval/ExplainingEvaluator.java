package com.g2forge.reassert.express.eval;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.error.UnreachableCodeError;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.eval.error.EvalFailedException;
import com.g2forge.reassert.express.eval.operation.IArgumentDescriptor;
import com.g2forge.reassert.express.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.eval.value.IValueSystem;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.ILiteral;
import com.g2forge.reassert.express.model.environment.IEnvironment;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.ExplainedOperation;
import com.g2forge.reassert.express.model.operation.IExplainedOperation;
import com.g2forge.reassert.express.model.operation.IExplainedOperation.Argument;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.operation.ImpliesExplainedOperation;
import com.g2forge.reassert.express.model.operation.ZeroExplainedOperation;
import com.g2forge.reassert.express.model.variable.ExplainedClosure;
import com.g2forge.reassert.express.model.variable.ExplainedVariable;
import com.g2forge.reassert.express.model.variable.IClosure;
import com.g2forge.reassert.express.model.variable.IExplainedClosure;
import com.g2forge.reassert.express.model.variable.IVariable;

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

			final boolean isImplies = BooleanOperation.Operator.IMPLIES.equals(expression.getOperator());
			final IOperatorDescriptor<Value> operatorDescriptor = getOperationSystem().getDescriptor(expression.getOperator());
			operatorDescriptor.validate(expression).throwIfInvalid();

			final IValueSystem<Value> valueSystem = getValueSystem();

			final List<? extends IExpression<Name, Value>> arguments = expression.getArguments();
			final List<IExplainedOperation.Argument<Value>> evaluated = new ArrayList<>();
			int zeroIndex = -1;
			final Set<Value> identities = new LinkedHashSet<>();
			for (int i = 0; i < arguments.size(); i++) {
				final IExplained<Value> result;
				try {
					result = context.eval(arguments.get(i));
				} catch (Throwable throwable) {
					throw new RuntimeException(String.format("Failed to evaluate %1$s!", expression.getOperator()), throwable);
				}

				final IArgumentDescriptor<Value> argumentDescriptor = operatorDescriptor.getArgument(i);
				final IOptional<? extends Value> zero = argumentDescriptor.getZeroInput();
				final IOptional<? extends Value> identity = argumentDescriptor.getIdentity();
				final IExplained.Relevance relevance;
				if ((zeroIndex == -1) && !zero.isEmpty() && valueSystem.isEqual(result.get(), zero.get())) {
					zeroIndex = i;
					relevance = IExplained.Relevance.Dominant;
				} else if (zeroIndex != -1) relevance = IExplained.Relevance.Unevaluated;
				else if (!identity.isEmpty() && valueSystem.isEqual(result.get(), identity.get())) relevance = IExplained.Relevance.Identity;
				else relevance = IExplained.Relevance.Combined;

				evaluated.add(new IExplainedOperation.Argument<>(relevance, result));
				if (identity.isNotEmpty()) identities.add(identity.get());
			}

			if ((zeroIndex != -1) && !isImplies) return new ZeroExplainedOperation<Value>(expression.getOperator(), operatorDescriptor.getArgument(zeroIndex).getZeroOutput().get(), evaluated);

			final Stream<Value> stream = evaluated.stream().map(IExplainedOperation.Argument::getExplained).map(IExplained::get);
			final Value reduced;
			if (identities.size() != 1) reduced = stream.reduce(operatorDescriptor::combine).get();
			else reduced = stream.reduce(HCollection.getOne(identities), operatorDescriptor::combine);

			final IFunction1<? super Value, ? extends Value> summarizer = operatorDescriptor.getSummarizer();
			final Value result = summarizer == null ? reduced : summarizer.apply(reduced);

			if (isImplies) {
				final Value FALSE = getOperationSystem().getDescriptor(BooleanOperation.Operator.AND).getArgument(0).getZeroInput().get();
				final boolean dominated = getValueSystem().isEqual(FALSE, evaluated.get(0).getExplained().get());
				final Argument<Value> premise = dominated ? evaluated.get(0).toBuilder().relevance(IExplained.Relevance.Dominant).build() : evaluated.get(0);
				return new ImpliesExplainedOperation<>(premise, evaluated.get(1), result);
			}
			final IOptional<? extends Value> identity = identities.size() == 1 ? NullableOptional.of(HCollection.getOne(identities)) : NullableOptional.empty();
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
		builder.fallback((e, c) -> {
			throw new EvalFailedException(e);
		});
		return builder.build();
	}
}
