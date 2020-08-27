package com.g2forge.reassert.reassert;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.convert.ReportRenderer;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.expression.explain.convert.ExplanationMode;
import com.g2forge.reassert.reassert.convert.TestReportRenderer;
import com.g2forge.reassert.reassert.summary.ReassertSummarizer;
import com.g2forge.reassert.reassert.summary.model.ReportSummary;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class ATestReassertSummarizer extends ATestFromList {
	protected static enum Output {
		Artifacts,
		Findings;
	}

	protected static class TestSummarizer extends ReassertSummarizer {
		public TestSummarizer(IContext context) {
			super(context);
		}

		protected IFunction1<? super ExplanationMode, ? extends ReportRenderer> createRendererFactory() {
			return TestReportRenderer::new;
		}
	}

	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final TestSummarizer summarizer = new TestSummarizer(ReassertContext.getContext());

	protected void assertOutput(final String name, final Output output, final ReportSummary summary) throws EnumException {
		final ReassertSummarizer summarizer = getSummarizer();
		final ByteArrayDataSink sink = new ByteArrayDataSink();
		switch (output) {
			case Artifacts:
				summarizer.renderArtifacts(summary, sink);
				break;
			case Findings:
				summarizer.renderFindings(summary, sink);
				break;
			default:
				throw new EnumException(Output.class, output);
		}
		HAssert.assertEquals(new Resource(getClass(), name + "-output-" + output.name().toLowerCase() + ".csv"), sink.getStream().toString());
	}

	protected IReport test(final String name, final Output output) {
		final IReport report = load(name).getReport();
		final ReportSummary summary = getSummarizer().summarize(report);
		assertOutput(name, output, summary);
		return report;
	}
}
