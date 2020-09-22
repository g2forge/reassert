package com.g2forge.reassert.contract.v2.convert;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.contract.v2.model.ICTName;
import com.g2forge.reassert.contract.v2.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.convert.ExplanationMode;
import com.g2forge.reassert.express.v2.model.IExplained;

public interface IReportRenderContext extends ITextualRenderContext<Object, IReportRenderContext> {
	public ICloseable findingContext(ExpressionContextFinding context);

	public ExpressionContextFinding getFindingContext();

	public ExplanationMode getMode();

	public IReportRenderContext name(ICTName name);

	public IReportRenderContext render(IExplained<TermRelation> explained);
}
