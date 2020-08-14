package com.g2forge.reassert.reassert.test;

import org.junit.Test;

import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.reassert.ATestReassertSummarizer;
import com.g2forge.reassert.reassert.TestGraph;

public class TestReassert extends ATestReassertSummarizer {
	@Override
	protected TestGraph load(Artifact<ListCoordinates> artifact) {
		return new TestGraph(artifact);
	}

	@Test
	public void permissiveArtifacts() {
		test("permissive", Output.Artifacts);
	}

	@Test
	public void permissiveRisks() {
		test("permissive", Output.Risks);
	}
}
