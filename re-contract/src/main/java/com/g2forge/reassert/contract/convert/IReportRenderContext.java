package com.g2forge.reassert.contract.convert;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.contract.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.convert.ExplanationMode;
import com.g2forge.reassert.express.model.IExplained;

public interface IReportRenderContext extends ITextualRenderContext<Object, IReportRenderContext> {
	public ICloseable findingContext(ExpressionContextFinding context);

	public ExpressionContextFinding getFindingContext();

	public ExplanationMode getMode();

	public IReportRenderContext name(IContractComparisonName name);

	public IReportRenderContext render(IExplained<TermRelation> explained);
}
