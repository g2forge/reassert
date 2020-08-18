package com.g2forge.reassert.reassert.test;

import java.util.List;

import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.reassert.ATestReassertSummarizer;
import com.g2forge.reassert.reassert.TestGraph;
import com.g2forge.reassert.reassert.algorithm.IGraphVisitor;

public class TestReassert extends ATestReassertSummarizer {
	@Test
	public void gplpublicArtifacts() {
		final IReport report = test("gplpublic", Output.Artifacts);
		HAssert.assertEquals(Level.WARN, report.getMinLevel());
	}

	@Test
	public void gplpublicFindings() {
		final IReport report = test("gplpublic", Output.Findings);
		HAssert.assertEquals(Level.WARN, report.getMinLevel());
	}

	@Test
	public void gplprivateArtifacts() {
		final IReport report = test("gplprivate", Output.Artifacts);
		HAssert.assertEquals(Level.INFO, report.getMinLevel());
	}

	@Test
	public void gplprivateFindings() {
		final IReport report = test("gplprivate", Output.Findings);
		HAssert.assertEquals(Level.INFO, report.getMinLevel());
	}

	@Override
	protected TestGraph load(Artifact<ListCoordinates> artifact) {
		return new TestGraph(artifact, (List<IGraphVisitor>) null);
	}

	@Test
	public void nousageArtifacts() {
		final IReport report = test("nousage", Output.Artifacts);
		HAssert.assertEquals(Level.ERROR, report.getMinLevel());
	}

	@Test
	public void nousageFindings() {
		final IReport report = test("nousage", Output.Findings);
		HAssert.assertEquals(Level.ERROR, report.getMinLevel());
	}

	@Test
	public void permissiveArtifacts() {
		final IReport report = test("permissive", Output.Artifacts);
		HAssert.assertEquals(Level.WARN, report.getMinLevel());
	}

	@Test
	public void permissiveFindings() {
		final IReport report = test("permissive", Output.Findings);
		HAssert.assertEquals(Level.WARN, report.getMinLevel());
	}
}
