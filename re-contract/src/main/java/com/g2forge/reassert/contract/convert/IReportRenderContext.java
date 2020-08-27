package com.g2forge.reassert.contract.convert;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.contract.convert.IReportRenderContext;
import com.g2forge.reassert.contract.model.IExpressionContext;
import com.g2forge.reassert.expression.explain.convert.ExplanationMode;
import com.g2forge.reassert.expression.explain.model.IExplained;

public interface IReportRenderContext extends ITextualRenderContext<Object, IReportRenderContext> {
	public ICloseable findingContext(IExpressionContext findingContext);

	public IExpressionContext getFindingContext();

	public IReportRenderContext render(IExplained<?> explained);
	
	public ExplanationMode getMode();
}
