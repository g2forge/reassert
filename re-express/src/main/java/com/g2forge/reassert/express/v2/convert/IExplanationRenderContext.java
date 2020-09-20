package com.g2forge.reassert.express.v2.convert;

import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.express.v2.model.IExplained;

public interface IExplanationRenderContext<Name, Value> extends ITextualRenderContext<IExplained<Value>, IExplanationRenderContext<Name, Value>> {
	public ExplanationMode getMode();

	public IExplanationRenderContext<Name, Value> name(Name name);
	
	public IExplanationRenderContext<Name, Value> value(Value value);
}
