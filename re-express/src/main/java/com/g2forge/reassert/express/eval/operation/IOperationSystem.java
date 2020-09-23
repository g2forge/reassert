package com.g2forge.reassert.express.eval.operation;

import com.g2forge.reassert.express.model.operation.IOperation;

public interface IOperationSystem<Value> {
	public IOperatorDescriptor<Value> getDescriptor(IOperation.IOperator operator);

	public IOperatorRendering getRendering(IOperation.IOperator operator);
}
