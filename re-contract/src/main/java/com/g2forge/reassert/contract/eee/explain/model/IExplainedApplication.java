package com.g2forge.reassert.contract.eee.explain.model;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public interface IExplainedApplication<T> extends IExplainedValue<T> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Argument {
		protected final boolean relevant;

		protected final IExplained<?> argument;
	}
}
