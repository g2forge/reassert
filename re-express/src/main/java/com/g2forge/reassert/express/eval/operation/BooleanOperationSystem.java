package com.g2forge.reassert.express.eval.operation;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.BooleanOperation.Operator;

public class BooleanOperationSystem extends ABooleanOperationSystem<Boolean> implements ISingleton {
	private static final BooleanOperationSystem INSTANCE = new BooleanOperationSystem();

	public static BooleanOperationSystem create() {
		return INSTANCE;
	}

	protected BooleanOperationSystem() {}

	@Override
	protected IOperatorDescriptor<Boolean> computeDescriptor(Operator operator) {
		switch (operator) {
			case NOT:
				return new UniformOperatorDescriptor<Boolean>(null, null, v -> !v);
			case OR:
				return new UniformOperatorDescriptor<Boolean>(true, false, null) {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return left || right;
					}
				};
			case AND:
				return new UniformOperatorDescriptor<Boolean>(false, true, null) {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return left || right;
					}
				};
			case XOR:
				return new UniformOperatorDescriptor<Boolean>(null, false, null) {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return left ^ right;
					}
				};
			case IMPLIES:
				return new ADifferentiatedOperatorDescriptor<Boolean>(new ArgumentDescriptor<>(false, true, true), new ArgumentDescriptor<>(true, null)) {
					@Override
					public Boolean combine(Boolean left, Boolean right) {
						return (!left) || right;
					}
				};
			default:
				throw new EnumException(BooleanOperation.Operator.class, operator);
		}
	}
}
