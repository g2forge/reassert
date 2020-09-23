package com.g2forge.reassert.express.model.operation;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ZeroExplainedOperation<Value> implements IExplainedOperation<Value> {
	public static class ZeroExplainedOperationBuilder<Value> implements IExplainedOperationBuilder<Value, ZeroExplainedOperationBuilder<Value>, ZeroExplainedOperation<Value>> {}

	protected final IOperation.IOperator operator;

	protected final Value zero;

	@Singular
	protected final List<Argument<Value>> arguments;

	@SafeVarargs
	public ZeroExplainedOperation(IOperation.IOperator operator, Value zero, Argument<Value>... arguments) {
		this(operator, zero, HCollection.asList(arguments));
	}

	@Override
	public Value getValue() {
		return getZero();
	}
}
