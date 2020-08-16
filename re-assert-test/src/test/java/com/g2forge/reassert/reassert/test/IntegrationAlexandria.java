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
import com.g2forge.reassert.reassert.algorithm.ReassertLicenseVisitor;

import lombok.AccessLevel;
import lombok.Getter;

public class IntegrationAlexandria {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TestGraph testGraph = new TestGraph(new Artifact<>(null, new GitCoordinates(null, "https://github.com/g2forge/alexandria.git", null)), HCollection.emptyList());

	@Test
	public void complete() {
		HAssert.assertEquals(new Resource(getClass(), "alexandria-complete.dot"), TestVisualizer.create().visualize(getTestGraph().getGraph()));
	}

	@Test
	public void licenses() {
		final Graph<IVertex, IEdge> graph = HReassertModel.clone(getTestGraph().getGraph());
		new ReassertLicenseVisitor().accept(graph);
		HAssert.assertEquals(new Resource(getClass(), "alexandria-licenses.dot"), TestVisualizer.create().visualize(HReassertModel.asLicenseGraph(graph)));
	}
}