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
public class BooleanOperation<Name, Value> implements IOperation<Name, Value> {
	public static class BooleanOperationBuilder<Name, Value> implements IOperationBuilder<Name, Value, BooleanOperationBuilder<Name, Value>, BooleanOperation<Name, Value>> {}

	public enum Operator implements IOperator {
		NOT {
			@Override
			public IValidation validate(List<? extends IExpression<?, ?>> arguments) {
				return new OperatorArgumentsValidation(arguments.size() == 1);
			}
		},
		AND,
		OR,
		XOR,
		IMPLIES {
			@Override
			public IValidation validate(List<? extends IExpression<?, ?>> arguments) {
				return new OperatorArgumentsValidation(arguments.size() == 2);
			}
		};

		@Override
		public <Name, Value> IOperationBuilder<Name, Value, ?, ?> builder() {
			return BooleanOperation.<Name, Value>builder().operator(this);
		}
	}

	@SafeVarargs
	public static <Name, Value> BooleanOperation<Name, Value> and(IExpression<Name, Value>... arguments) {
		return new BooleanOperation<>(Operator.AND, arguments);
	}

	public static <Name, Value> BooleanOperation<Name, Value> implies(IExpression<Name, Value> premise, IExpression<Name, Value> conclusion) {
		return new BooleanOperation<>(Operator.IMPLIES, premise, conclusion);
	}

	public static <Name, Value> BooleanOperation<Name, Value> not(IExpression<Name, Value> argument) {
		return new BooleanOperation<>(Operator.NOT, argument);
	}

	@SafeVarargs
	public static <Name, Value> BooleanOperation<Name, Value> or(IExpression<Name, Value>... arguments) {
		return new BooleanOperation<>(Operator.OR, arguments);
	}

	@SafeVarargs
	public static <Name, Value> BooleanOperation<Name, Value> xor(IExpression<Name, Value>... arguments) {
		return new BooleanOperation<>(Operator.XOR, arguments);
	}

	protected final Operator operator;

	@Singular
	protected final List<IExpression<Name, Value>> arguments;

	@SafeVarargs
	public BooleanOperation(Operator operator, IExpression<Name, Value>... arguments) {
		this(operator, HCollection.asList(arguments));
	}
}
