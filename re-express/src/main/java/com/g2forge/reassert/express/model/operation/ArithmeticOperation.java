package com.g2forge.reassert.express.model.operation;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.validate.IValidation;
import com.g2forge.reassert.express.model.IExpression;

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
		ADD,
		SUBTRACT {
			@Override
			public IValidation validate(List<? extends IExpression<?, ?>> arguments) {
				return new OperatorArgumentsValidation(arguments.size() == 2);
			}
		},
		MULTIPLY,
		DIVIDE {
			@Override
			public IValidation validate(List<? extends IExpression<?, ?>> arguments) {
				return new OperatorArgumentsValidation(arguments.size() == 2);
			}
		};

		@Override
		public <Name, Value> IOperationBuilder<Name, Value, ?, ?> builder() {
			return ArithmeticOperation.<Name, Value>builder().operator(this);
		}
	}

	@SafeVarargs
	public static <Name, Value> ArithmeticOperation<Name, Value> add(IExpression<Name, Value>... arguments) {
		return new ArithmeticOperation<>(Operator.ADD, arguments);
	}

	public static <Name, Value> ArithmeticOperation<Name, Value> divide(IExpression<Name, Value> numberator, IExpression<Name, Value> denominator) {
		return new ArithmeticOperation<>(Operator.DIVIDE, numberator, denominator);
	}

	@SafeVarargs
	public static <Name, Value> ArithmeticOperation<Name, Value> multiply(IExpression<Name, Value>... arguments) {
		return new ArithmeticOperation<>(Operator.MULTIPLY, arguments);
	}

	public static <Name, Value> ArithmeticOperation<Name, Value> subtract(IExpression<Name, Value> minuend, IExpression<Name, Value> subtrahend) {
		return new ArithmeticOperation<>(Operator.SUBTRACT, minuend, subtrahend);
	}

	protected final Operator operator;

	@Singular
	protected final List<IExpression<Name, Value>> arguments;

	@SafeVarargs
	public ArithmeticOperation(Operator operator, IExpression<Name, Value>... arguments) {
		this(operator, HCollection.asList(arguments));
	}
}
