package com.g2forge.reassert.express.v2.model.operation;

import java.util.List;

import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.IExplainedValue;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public interface IExplainedOperation<Value> extends IExplainedValue<Value> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Argument<Value> {
		protected final Relevance relevance;

		protected final IExplained<Value> explained;
	}

	public List<Argument<Value>> getArguments();

	public IOperation.IOperator getOperator();
}
