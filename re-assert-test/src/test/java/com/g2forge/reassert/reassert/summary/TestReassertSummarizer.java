package com.g2forge.reassert.reassert.summary;

import java.util.List;

import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.license.GeneralLicense;
import com.g2forge.reassert.core.model.contract.usage.GeneralUsage;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.mock.MockCoordinates;
import com.g2forge.reassert.reassert.ATestReassertSummarizer;
import com.g2forge.reassert.reassert.TestGraph;
import com.g2forge.reassert.reassert.model.report.TestFinding;
import com.g2forge.reassert.reassert.model.report.TestRuleFinding;
import com.g2forge.reassert.reassert.summary.model.ArtifactSummary;
import com.g2forge.reassert.reassert.summary.model.FindingSummary;
import com.g2forge.reassert.reassert.summary.model.ReportSummary;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

public class TestReassertSummarizer extends ATestReassertSummarizer {
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

	@Override
	protected TestGraph load(Artifact<ListCoordinates> artifact) {
		return new TestGraph(artifact);
	}

	@Test
	public void rendering() {
		final ReportSummary.ReportSummaryBuilder builder = ReportSummary.builder();
		{
			final ArtifactSummary.ArtifactSummaryBuilder a = ArtifactSummary.builder().level(Level.WARN).artifact(new MockCoordinates("A"));
			a.finding(new TestFinding(Level.WARN, "a finding"));
			a.usage(GeneralUsage.builder().name("some usage").build());
			a.license(GeneralLicense.builder().name("license 0").build());
			builder.artifact(a.build());
		}
		{
			final ArtifactSummary.ArtifactSummaryBuilder b = ArtifactSummary.builder().level(Level.ERROR).artifact(new MockCoordinates("B"));
			b.finding(new TestFinding(Level.ERROR, "finding 0"));
			b.finding(new TestFinding(Level.INFO, "finding 1"));
			b.usage(GeneralUsage.builder().name("some usage").build());
			b.license(GeneralLicense.builder().name("license 1").build());
			b.license(GeneralLicense.builder().name("license 2").build());
			b.path(TestGraphPath.builder().vertex(new MockCoordinates("A")).vertex(new MockCoordinates("B")).build());
			builder.artifact(b.build());
		}
		{
			final FindingSummary.FindingSummaryBuilder finding = FindingSummary.builder();
			finding.finding(new TestRuleFinding(Level.INFO, "Some risk"));
			finding.path(TestGraphPath.builder().vertex(new MockCoordinates("C")).build());
			builder.finding(finding.build());
		}

		final ReportSummary summary = builder.build();

		assertOutput("rendering", Output.Artifacts, summary);
		assertOutput("rendering", Output.Findings, summary);
	}

	@Test
	public void summaryArtifacts() {
		test("summary", Output.Artifacts);
	}

	@Test
	public void summaryFindings() {
		test("summary", Output.Findings);
	}
}
