package com.g2forge.reassert.reassert.summary;

import java.util.List;

import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.mock.MockCoordinates;
import com.g2forge.reassert.reassert.ATestFromList;
import com.g2forge.reassert.reassert.TestGraph;
import com.g2forge.reassert.reassert.ReassertContext;
import com.g2forge.reassert.reassert.convert.ReportRenderer;
import com.g2forge.reassert.reassert.convert.TestReportRenderer;
import com.g2forge.reassert.reassert.model.contract.TestLicense;
import com.g2forge.reassert.reassert.model.contract.TestUsage;
import com.g2forge.reassert.reassert.model.finding.TestFinding;
import com.g2forge.reassert.reassert.model.finding.TestRiskFinding;
import com.g2forge.reassert.reassert.summary.convert.SummaryModule;
import com.g2forge.reassert.reassert.summary.model.ArtifactSummary;
import com.g2forge.reassert.reassert.summary.model.ReportSummary;
import com.g2forge.reassert.reassert.summary.model.RiskSummary;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

public class TestReassertSummarizer extends ATestFromList {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	protected static class TestGraphPath implements GraphPath<IVertex, IEdge> {
		@Singular("vertex")
		protected final List<IVertex> vertexList;

		@Override
		public IVertex getEndVertex() {
			final List<IVertex> vertexList = getVertexList();
			return vertexList.get(vertexList.size() - 1);
		}

		@Override
		public Graph<IVertex, IEdge> getGraph() {
			throw new UnsupportedOperationException();
		}

		@Override
		public IVertex getStartVertex() {
			return getVertexList().get(0);
		}

		@Override
		public double getWeight() {
			throw new UnsupportedOperationException();
		}
	}

	protected static class TestSummarizer extends ReassertSummarizer {
		public TestSummarizer(IContext context) {
			super(context);
		}

		@Override
		protected SummaryModule createSummaryModule() {
			return new SummaryModule(getContext()) {
				private static final long serialVersionUID = 4626951969629933615L;

				@Override
				protected ReportRenderer createReportRenderer() {
					return new TestReportRenderer();
				}
			};
		}
	}

	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final ReassertSummarizer summarizer = new TestSummarizer(ReassertContext.getContext());

	protected void assertOutput(final String name, final ReportSummary summary) {
		final ReassertSummarizer summarizer = getSummarizer();

		{
			final ByteArrayDataSink sink = new ByteArrayDataSink();
			summarizer.renderArtifacts(summary, sink);
			HAssert.assertEquals(new Resource(getClass(), name + "-output-artifacts.csv"), sink.getStream().toString());
		}

		{
			final ByteArrayDataSink sink = new ByteArrayDataSink();
			summarizer.renderRisks(summary, sink);
			HAssert.assertEquals(new Resource(getClass(), name + "-output-risks.csv"), sink.getStream().toString());
		}
	}

	@Override
	protected TestGraph load(Artifact<ListCoordinates> artifact) {
		return new TestGraph(artifact);
	}

	@Test
	public void rendering() {
		final ReportSummary.ReportSummaryBuilder summary = ReportSummary.builder();
		{
			final ArtifactSummary.ArtifactSummaryBuilder a = ArtifactSummary.builder().level(Level.WARN).artifact(new MockCoordinates("A"));
			a.finding(new TestFinding(Level.WARN, "a finding"));
			a.usage(new TestUsage("some usage", null));
			a.license(new TestLicense("license 0", null, null));
			summary.artifact(a.build());
		}
		{
			final ArtifactSummary.ArtifactSummaryBuilder b = ArtifactSummary.builder().level(Level.ERROR).artifact(new MockCoordinates("B"));
			b.finding(new TestFinding(Level.ERROR, "finding 0"));
			b.finding(new TestFinding(Level.INFO, "finding 1"));
			b.usage(new TestUsage("some usage", null));
			b.license(new TestLicense("license 1", null, null));
			b.license(new TestLicense("license 2", null, null));
			b.path(TestGraphPath.builder().vertex(new MockCoordinates("A")).vertex(new MockCoordinates("B")).build());
			summary.artifact(b.build());
		}
		{
			final RiskSummary.RiskSummaryBuilder risk = RiskSummary.builder();
			risk.level(Level.INFO).artifact(new MockCoordinates("C"));
			risk.risk(new TestRiskFinding(Level.INFO, "Some risk"));
			summary.risk(risk.build());
		}
		assertOutput("rendering", summary.build());
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
