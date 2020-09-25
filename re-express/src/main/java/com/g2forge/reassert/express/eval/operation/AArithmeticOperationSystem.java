package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.reassert.express.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.model.operation.ArithmeticOperation.Operator;

public abstract class AArithmeticOperationSystem extends AOperationSystem<Integer, ArithmeticOperation.Operator> {
	protected AArithmeticOperationSystem() {
		super(ArithmeticOperation.Operator.class);
	}

	@Override
	protected IOperatorRendering computeRendering(Operator operator) {
		switch (operator) {
			case ADD:
				return new EnumOperatorRendering<>(operator, "added to");
			case SUBTRACT:
				return new EnumOperatorRendering<>(operator, "subtracted from");
			case MULTIPLY:
				return new EnumOperatorRendering<>(operator, "multiplied by");
			case DIVIDE:
				return new EnumOperatorRendering<>(operator, "divided by");
			default:
				throw new EnumException(ArithmeticOperation.Operator.class, operator);
		}
	}
}
