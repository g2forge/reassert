package com.g2forge.reassert.express.v2.model.variable;

import java.util.List;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.express.v2.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ExplainedClosure<Name, Value> implements IExplainedClosure<Name, Value> {
	public static class ExplainedClosureBuilder<Name, Value> implements IBuilder<ExplainedClosure<Name, Value>> {
		public ExplainedClosureBuilder<Name, Value> binding$(Relevance relevance, IVariable<Name, Value> variable, IOptional<? extends IExplained<Value>> result) {
			return binding(new IExplainedClosure.Binding<>(relevance, variable, result));
		}
	}

	protected final IExplained<Value> expression;

	@Singular
	protected final List<IExplainedClosure.Binding<Name, Value>> bindings;

	@Override
	public Value get() {
		return getExpression().get();
	}
}
