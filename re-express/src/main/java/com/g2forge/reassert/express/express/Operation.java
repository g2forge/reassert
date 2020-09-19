package com.g2forge.reassert.express.express;

import java.util.List;

import com.g2forge.alexandria.java.function.builder.IBuilder;

import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Operation<T> implements IOperation<T> {
	public static class OperationBuilder<T> implements IBuilder<Operation<T>> {
		public OperationBuilder<T> argument$(String name, T value) {
			return argument(new Literal<>(name, value));
		}

		public OperationBuilder<T> argument$(T value) {
			return argument$(null, value);
		}
	}

	@Getter
	@RequiredArgsConstructor
	public enum Operator {
		AND,
		OR,
		NOT;

		public <T> Operation.OperationBuilder<T> builder() {
			return Operation.<T>builder().operator(this);
		}
	}

	protected final Operator operator;

	@Singular
	protected final List<IExpression<T>> arguments;
}
