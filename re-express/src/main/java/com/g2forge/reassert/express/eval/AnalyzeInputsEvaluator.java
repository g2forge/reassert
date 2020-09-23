package com.g2forge.reassert.express.eval;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.express.eval.error.EvalFailedException;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.ILiteral;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.variable.IClosure;
import com.g2forge.reassert.express.model.variable.IVariable;

public class AnalyzeInputsEvaluator<Name, Value> extends AEvaluator<Name, Value, Set<IVariable<Name, Value>>, AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>>> {
	@Override
	protected BasicContext<Name, Value, Set<IVariable<Name, Value>>> createContext() {
		return new AEvaluator.BasicContext<>(getEvaluator());
	}

	@Override
	protected IFunction2<IExpression<Name, Value>, AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>>, Set<IVariable<Name, Value>>> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<Name, Value>, AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>>, Set<IVariable<Name, Value>>> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(ILiteral.class, AEvaluator.BasicContext.class, (e, c) -> Collections.emptySet());
		builder.add(IOperation.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IOperation<Name, Value> expression = (IOperation<Name, Value>) e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>> context = (AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>>) c;

			return expression.getArguments().stream().flatMap(argument -> context.eval(argument).stream()).collect(Collectors.toCollection(LinkedHashSet::new));
		});
		builder.add(IClosure.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IClosure<Name, Value> expression = (IClosure<Name, Value>) e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>> context = (AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>>) c;

			try (final ICloseable environment = context.environment(expression.getEnvironment())) {
				return context.eval(expression.getExpression());
			}
		});
		builder.add(IVariable.class, AEvaluator.BasicContext.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IVariable<Name, Value> expression = e;
			@SuppressWarnings("unchecked")
			final AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>> context = (AEvaluator.BasicContext<Name, Value, Set<IVariable<Name, Value>>>) c;

			return context.getEnvironment().lookup(expression).isEmpty() ? HCollection.asSet(expression) : Collections.emptySet();
		});
		builder.fallback((e, c) -> {
			throw new EvalFailedException(e);
		});
		return builder.build();
	}
}
