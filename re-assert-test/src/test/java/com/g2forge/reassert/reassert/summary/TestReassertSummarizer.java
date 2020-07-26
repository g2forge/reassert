package com.g2forge.reassert.reassert.summary;

import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.mock.MockCoordinates;
import com.g2forge.reassert.reassert.ATestReassert;
import com.g2forge.reassert.reassert.ReassertContext;
import com.g2forge.reassert.reassert.algorithm.example.ExampleGraph;
import com.g2forge.reassert.reassert.summary.model.ArtifactSummary;
import com.g2forge.reassert.reassert.summary.model.ReportSummary;

import lombok.AccessLevel;
import lombok.Getter;

public class TestReassertSummarizer extends ATestReassert {
	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final ReassertSummarizer summarizer = new ReassertSummarizer(ReassertContext.getContext());

	@Test
	public void artifacts() {
		final ReportSummary.ReportSummaryBuilder summary = ReportSummary.builder();
		summary.artifact(ArtifactSummary.builder().level(Level.WARN).artifact(new MockCoordinates("A")).finding(null).build());
		summary.artifact(ArtifactSummary.builder().level(Level.ERROR).artifact(new MockCoordinates("B")).finding(null).build());
		assertOutput("artifacts", summary.build());
	}

	protected void assertOutput(final String name, final ReportSummary summary) {
		final ByteArrayDataSink sink = new ByteArrayDataSink();
		getSummarizer().renderArtifacts(summary, sink);
		HAssert.assertEquals(new Resource(getClass(), name + "-output.csv").read(false), sink.getStream().toString());
	}

	@Override
	protected ExampleGraph load(Artifact<ListCoordinates> artifact) {
		return new ExampleGraph(artifact);
	}

	@Test
	public void summary() {
		test("summary");
	}

	protected void test(final String name) {
		final IReport report = load(name).getReport();
		final ReportSummary summary = getSummarizer().summarize(report);
		assertOutput(name, summary);
	}
}
