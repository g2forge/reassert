package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.express.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.model.operation.ArithmeticOperation.Operator;

public class IntegerOperationSystem extends AArithmeticOperationSystem implements ISingleton {
	private static final IntegerOperationSystem INSTANCE = new IntegerOperationSystem();

	public static IntegerOperationSystem create() {
		return INSTANCE;
	}

	protected IntegerOperationSystem() {}

	@Override
	protected IOperatorDescriptor<Integer> computeDescriptor(Operator operator) {
		switch (operator) {
			case ADD:
				return new UniformOperatorDescriptor<Integer>(null, 0, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left + right;
					}
				};
			case SUBTRACT:
				return new UniformOperatorDescriptor<Integer>(null, null, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left - right;
					}
				};
			case MULTIPLY:
				return new UniformOperatorDescriptor<Integer>(0, 1, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left * right;
					}
				};
			case DIVIDE:
				return new UniformOperatorDescriptor<Integer>(null, null, null) {
					@Override
					public Integer combine(Integer left, Integer right) {
						return left / right;
					}
				};
			default:
				throw new EnumException(ArithmeticOperation.Operator.class, operator);
		}
	}
}
