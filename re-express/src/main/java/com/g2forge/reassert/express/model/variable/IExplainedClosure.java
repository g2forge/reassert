package com.g2forge.reassert.express.model.variable;

import java.util.Collection;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.reassert.express.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

public interface IExplainedClosure<Name, Value> extends IExplained<Value> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class Binding<Name, Value> implements IExplainedVariable<Name, Value> {
		protected final Relevance relevance;

		protected final IVariable<Name, Value> variable;

		protected final IOptional<? extends IExplained<Value>> explained;
	}

	public Collection<IExplainedClosure.Binding<Name, Value>> getBindings();

	public IExplained<Value> getExpression();
}
