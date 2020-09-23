package com.g2forge.reassert.express.model.operation;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ExplainedOperation<Value> implements IExplainedOperation<Value> {
	public static class ExplainedOperationBuilder<Value> implements IExplainedOperationBuilder<Value, ExplainedOperationBuilder<Value>, ExplainedOperation<Value>> {
		public ExplainedOperationBuilder<Value> identity$(Value value) {
			return identity(NullableOptional.of(value));
		}
	}

	protected final IOperation.IOperator operator;

	protected final Value value;

	@Builder.Default
	protected final IOptional<? extends Value> identity = NullableOptional.empty();

	@Singular
	protected final List<Argument<Value>> arguments;

	@SafeVarargs
	public ExplainedOperation(IOperation.IOperator operator, Value value, IOptional<Value> identity, Argument<Value>... arguments) {
		this(operator, value, identity, HCollection.asList(arguments));
	}
}
