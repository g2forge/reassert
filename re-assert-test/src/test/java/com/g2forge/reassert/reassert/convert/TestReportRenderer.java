package com.g2forge.reassert.reassert.convert;

import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.reassert.contract.analyze.convert.IReportRenderContext;
import com.g2forge.reassert.contract.analyze.convert.ReportRenderer;
import com.g2forge.reassert.contract.eee.explain.convert.ExplanationMode;
import com.g2forge.reassert.reassert.model.finding.TestFinding;
import com.g2forge.reassert.reassert.model.finding.TestRiskFinding;

import lombok.AccessLevel;
import lombok.Getter;

public class TestReportRenderer extends ReportRenderer {
	protected static class TestReportRendering extends ReportRendering {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<Object, IExplicitRenderable<? super IReportRenderContext>> builder) {
			super.extend(builder);
			builder.add(TestFinding.class, e -> c -> appendLevel(e, c).append(e.getMessage()));
			builder.add(TestRiskFinding.class, e -> c -> appendLevel(e, c).append(e.getDescription()));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> renderingStatic = new TestReportRendering();

	public TestReportRenderer() {
		super();
	}

	public TestReportRenderer(ExplanationMode mode) {
		super(mode);
	}

	@Override
	protected IRendering<? super Object, ? extends IReportRenderContext, ? extends IExplicitRenderable<? super IReportRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
