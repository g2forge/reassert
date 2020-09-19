package com.g2forge.reassert.express.v2.eval;

import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorDescriptor;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.constant.IConstant;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.operation.IOperation;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReductionRewriter<Name, Value> extends AEvaluator<Name, Value, IExpression<Name, Value>> implements IRewriter<Name, Value> {
	public enum Reduction {
		TrivialOperations;
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
	protected IFunction2<IExpression<Name, Value>, Context<Name, Value, IExpression<Name, Value>>, IExpression<Name, Value>> createEvaluator() {
		final TypeSwitch2.FunctionBuilder<IExpression<Name, Value>, Context<Name, Value, IExpression<Name, Value>>, IExpression<Name, Value>> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(IConstant.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IConstant<Name, Value> expression = e;
			return expression;
		});
		builder.add(IOperation.class, Context.class, (e, c) -> {
			@SuppressWarnings("unchecked")
			final IOperation<Name, Value> expression = e;

			if (getReductions().contains(Reduction.TrivialOperations)) {
				final List<? extends IExpression<Name, Value>> arguments = expression.getArguments();
				if (arguments.size() <= 1) {
					final IOperatorDescriptor<Value> descriptor = getOperationSystem().getDescriptor(expression.getOperator());
					if (arguments.isEmpty() && !descriptor.getIdentity().isEmpty()) return new Literal<>(descriptor.getIdentity().get());
					if (descriptor.getSummarizer() == null) return HCollection.getOne(arguments);
				}
			}

			return expression;
		});
		return builder.build();
	}
}
