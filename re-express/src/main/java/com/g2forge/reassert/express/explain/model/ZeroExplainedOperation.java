package com.g2forge.reassert.express.explain.model;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.builder.IBuilder;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ZeroExplainedOperation<T> implements IExplainedApplication<T> {
	public static class ZeroExplainedOperationBuilder<T> implements IBuilder<ZeroExplainedOperation<T>> {
		public ZeroExplainedOperationBuilder<T> argument$(boolean relevant, IExplained<?> argument) {
			argument(new Argument(relevant, argument));
			return this;
		}
	}

	protected final Object operator;

	protected final T zero;

	@Singular
	protected final List<Argument> arguments;

	public ZeroExplainedOperation(Object operator, T zero, Argument... arguments) {
		this(operator, zero, HCollection.asList(arguments));
	}

	@Override
	public T getValue() {
		return getZero();
	}
}
