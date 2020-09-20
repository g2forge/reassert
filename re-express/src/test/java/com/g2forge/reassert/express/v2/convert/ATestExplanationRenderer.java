package com.g2forge.reassert.express.v2.convert;

import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;
import com.g2forge.reassert.express.v2.model.IExplained;

public abstract class ATestExplanationRenderer<Name, Value> {
	protected abstract IOperationSystem< Value> getOperationSystem();
	
	protected abstract IValueSystem<? super Value> getValueSystem();

	protected String render(ExplanationMode mode, final IExplained<Value> explained) {
		return new ExplanationRenderer<Name, Value>(mode, getValueSystem(), getOperationSystem()).render(explained);
	}
}
