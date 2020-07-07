package com.g2forge.reassert.reassert.algorithm.example.alexandria;

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
import com.g2forge.reassert.reassert.algorithm.ReassertLicenseVisitor;
import com.g2forge.reassert.reassert.algorithm.example.ExampleGraph;
import com.g2forge.reassert.reassert.algorithm.example.ExampleVisualizer;

import lombok.AccessLevel;
import lombok.Getter;

public class TestAlexandria {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final ExampleGraph exampleGraph = new ExampleGraph(new Artifact<>(null, new GitCoordinates(null, "https://github.com/g2forge/alexandria.git", null)), HCollection.emptyList());

	@Test
	public void complete() {
		HAssert.assertEquals(new Resource(getClass(), "alexandriacomplete.dot"), ExampleVisualizer.create().visualize(getExampleGraph().getGraph()));
	}

	@Test
	public void licenses() {
		final Graph<IVertex, IEdge> graph = HReassertModel.clone(getExampleGraph().getGraph());
		new ReassertLicenseVisitor().accept(graph);
		HAssert.assertEquals(new Resource(getClass(), "alexandrialicenses.dot"), ExampleVisualizer.create().visualize(HReassertModel.asLicenseGraph(graph)));
	}
}
