package com.g2forge.reassert.express.v2.model.operation;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.express.v2.model.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ArithmeticOperation<Name, Value> implements IOperation<Name, Value> {
	public static class ArithmeticOperationBuilder<Name, Value> implements IOperationBuilder<Name, Value, ArithmeticOperationBuilder<Name, Value>, ArithmeticOperation<Name, Value>> {}

	public enum Operator implements IOperator {
		Add,
		Subtract;

		@Override
		public <Name, Value> IOperationBuilder<Name, Value, ?, ?> builder() {
			return ArithmeticOperation.<Name, Value>builder().operator(this);
		}
	}

	protected final Operator operator;

	@Singular
	protected final List<IExpression<Name, Value>> arguments;

	@SafeVarargs
	public ArithmeticOperation(Operator operator, IExpression<Name, Value>... arguments) {
		this(operator, HCollection.asList(arguments));
	}
}
