package com.g2forge.reassert.express.v2.eval.operation;

import com.g2forge.reassert.express.v2.model.operation.IOperation;

public interface IOperationSystem<Value> {
	public IOperatorDescriptor<Value> getDescriptor(IOperation.IOperator operator);

	public IOperatorRendering getRendering(IOperation.IOperator operator);
}
