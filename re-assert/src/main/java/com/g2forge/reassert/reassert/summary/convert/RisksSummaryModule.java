package com.g2forge.reassert.reassert.summary.convert;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.reassert.summary.model.RiskSummary;
import com.g2forge.reassert.term.analyze.convert.ReportRenderer;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;

public class RisksSummaryModule extends ASummaryModule {
	private static final long serialVersionUID = 840399436131646940L;

	public RisksSummaryModule(IContext context, IFunction1<? super ExplanationMode, ? extends ReportRenderer> rendererFactory) {
		super(context, rendererFactory);
	}
	
	protected JsonSerializer<?> modify(BeanDescription description, JsonSerializer<?> serializer) {
		if (RiskSummary.class.isAssignableFrom(description.getBeanClass())) return new RiskSummarySerializer(getRendererFactory().apply(ExplanationMode.Summarize));
		if (IFinding.class.isAssignableFrom(description.getBeanClass())) return new FindingSerializer(getRendererFactory().apply(ExplanationMode.Explain));
		return super.modify(description, serializer);
	}
}
