package com.g2forge.reassert.reassert.test;

import org.jgrapht.Graph;
import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.git.GitCoordinates;
import com.g2forge.reassert.reassert.TestGraph;
import com.g2forge.reassert.reassert.TestVisualizer;
import com.g2forge.reassert.standard.algorithm.StandardLicenseInheritanceVisitor;

import lombok.AccessLevel;
import lombok.Getter;

public class IntegrationReassert {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TestGraph testGraph = computeTestGraph();

	protected static TestGraph computeTestGraph() {
		return new TestGraph(new Artifact<>(null, new GitCoordinates(null, "https://github.com/g2forge/reassert.git", "0.1.0")), HCollection.emptyList());
	}

	@Test
	public void complete() {
		HAssert.assertEquals(new Resource(getClass(), "reassert-test-complete.dot"), TestVisualizer.create().visualize(getTestGraph().getGraph()));
	}

	@Test
	public void licenses() {
		final Graph<IVertex, IEdge> graph = HReassertModel.clone(getTestGraph().getGraph());
		new StandardLicenseInheritanceVisitor().accept(graph);
		HAssert.assertEquals(new Resource(getClass(), "reassert-test-licenses.dot"), TestVisualizer.create().visualize(HReassertModel.asLicenseGraph(graph)));
	}
}
