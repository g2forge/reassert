package com.g2forge.reassert.contract.eee.explain.model;

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
public class IdentityExplainedOperation<T> implements IExplainedApplication<T> {
	public static class IdentityExplainedOperationBuilder<T> implements IBuilder<IdentityExplainedOperation<T>> {
		public IdentityExplainedOperationBuilder<T> argument$(boolean relevant, IExplained<?> argument) {
			argument(new Argument(relevant, argument));
			return this;
		}
	}

	protected final T value;

	protected final Object operator;

	protected final T identity;

	@Singular
	protected final List<Argument> arguments;

	public IdentityExplainedOperation(T value, Object operator, T identity, Argument... arguments) {
		this(value, operator, identity, HCollection.asList(arguments));
	}
}
