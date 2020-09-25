package com.g2forge.reassert.express.model.operation;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.operation.IOperation.IOperator;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ImpliesExplainedOperation<Value> implements IExplainedOperation<Value> {
	public static class ImpliesExplainedOperationBuilder<Value> implements IBuilder<ImpliesExplainedOperation<Value>> {
		public ImpliesExplainedOperationBuilder<Value> conclusion$(IExplained.Relevance relevance, IExplained<Value> argument) {
			return conclusion(new Argument<>(relevance, argument));
		}

		public ImpliesExplainedOperationBuilder<Value> conclusion$(IExplained.Relevance relevance, Value argument) {
			return conclusion$(relevance, new Literal<>(argument));
		}

		public ImpliesExplainedOperationBuilder<Value> premise$(IExplained.Relevance relevance, IExplained<Value> argument) {
			return premise(new Argument<>(relevance, argument));
		}

		public ImpliesExplainedOperationBuilder<Value> premise$(IExplained.Relevance relevance, Value argument) {
			return premise$(relevance, new Literal<>(argument));
		}
	}

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final List<Argument<Value>> arguments = HCollection.asList(getPremise(), getConclusion());

	protected final Argument<Value> premise;

	protected final Argument<Value> conclusion;

	protected final Value value;

	@Override
	public IOperator getOperator() {
		return BooleanOperation.Operator.IMPLIES;
	}
}
