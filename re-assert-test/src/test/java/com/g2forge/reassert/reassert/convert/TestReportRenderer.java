package com.g2forge.reassert.reassert.convert;

import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.reassert.reassert.test.finding.TestFinding;
import com.g2forge.reassert.reassert.test.finding.TestRiskFinding;
import com.g2forge.reassert.term.analyze.convert.IReportRenderContext;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;

import lombok.AccessLevel;
import lombok.Getter;

public class TestReportRenderer extends ReportRenderer {
	protected static class TestReportRendering extends ReportRendering {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<Object, IExplicitRenderable<? super IReportRenderContext>> builder) {
			super.extend(builder);
			builder.add(TestFinding.class, e -> c -> c.append(e.getLevel()).append(": ").append(e.getMessage()));
			builder.add(TestRiskFinding.class, e -> c -> c.append(e.getLevel()).append(": ").append(e.getDescription()));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> renderingStatic = new TestReportRendering();

	public TestReportRenderer() {}

	public TestReportRenderer(ExplanationMode mode) {
		super(mode);
	}

	@Override
	protected IRendering<? super Object, ? extends IReportRenderContext, ? extends IExplicitRenderable<? super IReportRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
