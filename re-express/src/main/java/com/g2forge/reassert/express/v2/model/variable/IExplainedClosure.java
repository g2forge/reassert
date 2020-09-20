package com.g2forge.reassert.express.v2.model.variable;

import java.util.Collection;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.reassert.express.v2.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public interface IExplainedClosure<Name, Value> extends IExplained<Value> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Binding<Name, Value> {
		protected final Relevance relevance;

		protected final IVariable<Name, Value> variable;

		protected final IOptional<? extends IExplained<Value>> result;
	}

	public Collection<IExplainedClosure.Binding<Name, Value>> getBindings();

	public IExplained<Value> getExpression();
}
