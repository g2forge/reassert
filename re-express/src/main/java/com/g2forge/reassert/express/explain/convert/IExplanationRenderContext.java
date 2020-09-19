package com.g2forge.reassert.express.explain.convert;

import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.express.explain.model.IExplained;

public interface IExplanationRenderContext extends ITextualRenderContext<IExplained<?>, IExplanationRenderContext> {
	public ExplanationMode getMode();

	public IExplanationRenderContext name(Object name);
	
	public IExplanationRenderContext value(Object value);
}
