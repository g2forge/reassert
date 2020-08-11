package com.g2forge.reassert.reassert.convert;

import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.reassert.term.analyze.convert.IReportRenderContext;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;

import lombok.AccessLevel;
import lombok.Getter;

public class ReportRenderer extends com.g2forge.reassert.term.analyze.convert.ReportRenderer {
	protected static class ReportRendering extends com.g2forge.reassert.term.analyze.convert.ReportRenderer.ReportRendering {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<Object, IExplicitRenderable<? super IReportRenderContext>> builder) {
			super.extend(builder);
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> renderingStatic = new ReportRendering();

	public ReportRenderer() {}

	public ReportRenderer(ExplanationMode mode) {
		super(mode);
	}

	@Override
	protected IRendering<? super Object, ? extends IReportRenderContext, ? extends IExplicitRenderable<? super IReportRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
