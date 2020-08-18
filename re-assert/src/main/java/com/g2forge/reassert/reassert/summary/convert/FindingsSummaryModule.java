package com.g2forge.reassert.reassert.summary.convert;

import org.jgrapht.GraphPath;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.reassert.summary.model.FindingSummary;
import com.g2forge.reassert.term.analyze.convert.ReportRenderer;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;

public class FindingsSummaryModule extends ASummaryModule {
	private static final long serialVersionUID = 840399436131646940L;

	public FindingsSummaryModule(IContext context, IFunction1<? super ExplanationMode, ? extends ReportRenderer> rendererFactory) {
		super(context, rendererFactory);
	}

	protected JsonSerializer<?> modify(BeanDescription description, JsonSerializer<?> serializer) {
		if (FindingSummary.class.isAssignableFrom(description.getBeanClass())) return new FindingSummarySerializer(getRendererFactory().apply(ExplanationMode.Summarize));
		if (IFinding.class.isAssignableFrom(description.getBeanClass())) return new FindingSerializer(getRendererFactory().apply(ExplanationMode.Explain));
		if (GraphPath.class.isAssignableFrom(description.getBeanClass())) return new PathSerializer(false, getVertexDescriber());
		return super.modify(description, serializer);
	}
}
