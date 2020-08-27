package com.g2forge.reassert.expression.explain.convert;

import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.expression.explain.model.IExplained;

public interface IExplanationRenderContext extends ITextualRenderContext<IExplained<?>, IExplanationRenderContext> {
	public ExplanationMode getMode();

	public IExplanationRenderContext value(Object value);
}
