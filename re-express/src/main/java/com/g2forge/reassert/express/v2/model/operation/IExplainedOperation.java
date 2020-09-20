package com.g2forge.reassert.express.v2.model.operation;

import java.util.List;

import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.IExplainedValue;
import com.g2forge.reassert.express.v2.model.constant.Literal;

import lombok.Data;
import lombok.RequiredArgsConstructor;

public interface IExplainedOperation<Value> extends IExplainedValue<Value> {
	@Data
	@lombok.Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Argument<Value> {
		protected final IExplained.Relevance relevance;

		protected final IExplained<Value> explained;
	}

	public interface IExplainedOperationBuilder<Value, Builder extends IExplainedOperationBuilder<Value, Builder, Built>, Built extends IExplainedOperation<Value>> extends IBuilder<Built> {
		public Builder argument(Argument<Value> argument);

		public default Builder argument$(IExplained.Relevance relevance, IExplained<Value> argument) {
			return argument(new Argument<>(relevance, argument));
		}
		
		public default Builder argument$(IExplained.Relevance relevance, Value argument) {
			return argument$(relevance, new Literal<>(argument));
		}
	}

	public List<Argument<Value>> getArguments();

	public IOperation.IOperator getOperator();
}
