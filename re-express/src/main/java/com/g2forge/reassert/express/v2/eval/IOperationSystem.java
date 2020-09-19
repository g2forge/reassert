package com.g2forge.reassert.express.v2.eval;

import com.g2forge.reassert.express.v2.model.IOperation;

public interface IOperationSystem<Value> {
	public IOperatorDescriptor<Value> getDescriptor(IOperation.IOperator operator);
}
