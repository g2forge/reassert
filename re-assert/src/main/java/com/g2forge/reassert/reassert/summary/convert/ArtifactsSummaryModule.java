package com.g2forge.reassert.reassert.summary.convert;

import org.jgrapht.GraphPath;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.contract.analyze.convert.ReportRenderer;
import com.g2forge.reassert.contract.eee.explain.convert.ExplanationMode;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.report.IFinding;

public class ArtifactsSummaryModule extends ASummaryModule {
	private static final long serialVersionUID = 840399436131646940L;

	public ArtifactsSummaryModule(IContext context, IFunction1<? super ExplanationMode, ? extends ReportRenderer> rendererFactory) {
		super(context, rendererFactory);
	}

	protected JsonSerializer<?> modify(BeanDescription description, JsonSerializer<?> serializer) {
		if (IFinding.class.isAssignableFrom(description.getBeanClass())) return new FindingSerializer(getRendererFactory().apply(ExplanationMode.Summarize));
		if (GraphPath.class.isAssignableFrom(description.getBeanClass())) return new PathSerializer(true, getVertexDescriber());
		return super.modify(description, serializer);
	}
}
