package com.g2forge.reassert.reassert.algorithm.usage;

import org.junit.Test;

import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.reassert.ATestReassert;
import com.g2forge.reassert.reassert.TestGraph;
import com.g2forge.reassert.standard.algorithm.StandardUsageAssignmentVisitor;
import com.g2forge.reassert.standard.algorithm.propogate.StandardUsagePropogation;

public class TestStandardUsageAssignmentVisitor extends ATestReassert {
	@Test
	public void commercial() {
		test("commercial");
	}

	@Override
	protected TestGraph load(final Artifact<ListCoordinates> artifact) {
		return new TestGraph(artifact, new StandardUsageAssignmentVisitor(StandardUsagePropogation.create()));
	}

	@Test
	public void unspecified() {
		test("unspecified");
	}
}
