package com.g2forge.reassert.express.eval.operation;

import com.g2forge.reassert.express.model.operation.BooleanOperation;

public abstract class ABooleanOperationSystem<Value> extends AOperationSystem<Value, BooleanOperation.Operator> {
	public ABooleanOperationSystem() {
		super(BooleanOperation.Operator.class);
	}

	@Override
	protected IOperatorRendering computeRendering(BooleanOperation.Operator operator) {
		switch (getOperator(operator)) {
			case IMPLIES:
				return new EnumOperatorRendering<>(getOperator(operator), "implied by");
			default:
				return new EnumOperatorRendering<>(getOperator(operator), getOperator(operator).name().toLowerCase() + "-ed with");
		}
	}
}
