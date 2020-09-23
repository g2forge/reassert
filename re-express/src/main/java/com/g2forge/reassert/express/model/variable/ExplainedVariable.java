package com.g2forge.reassert.express.model.variable;

import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.reassert.express.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ExplainedVariable<Name, Value> implements IExplainedVariable<Name, Value> {
	protected final IVariable<Name, Value> variable;

	protected final IOptional<? extends IExplained<Value>> explained;
}
