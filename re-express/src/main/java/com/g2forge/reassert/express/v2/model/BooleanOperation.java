package com.g2forge.reassert.express.v2.model;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class BooleanOperation<Name, Value> implements IOperation<Name, Value> {
	public static class BooleanOperationBuilder<Name, Value> implements IOperationBuilder<Name, Value, BooleanOperationBuilder<Name, Value>, BooleanOperation<Name, Value>> {}

	public enum Operator implements IOperator {
		Not {
			@Override
			public boolean isValid(List<? extends IExpression<?, ?>> arguments) {
				return arguments.size() == 1;
			}
		},
		And,
		Or,
		Xor;

		@Override
		public <Name, Value> IOperationBuilder<Name, Value, ?, ?> builder() {
			return BooleanOperation.<Name, Value>builder().operator(this);
		}
	}

	protected final Operator operator;

	@Singular
	protected final List<IExpression<Name, Value>> arguments;

	@SafeVarargs
	public BooleanOperation(Operator operator, IExpression<Name, Value>... arguments) {
		this(operator, HCollection.asList(arguments));
	}
}
