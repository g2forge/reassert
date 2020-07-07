package com.g2forge.reassert.core.algorithm;

import org.jgrapht.Graph;
import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.algorithm.ReassertGraphIsomorphism;
import com.g2forge.reassert.core.model.Copy;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.mock.MockCoordinates;

public class TestReassertGraphIsomorphism {
	@Test
	public void copyUndirected() {
		final Graph<IVertex, IEdge> graph0 = HReassertModel.createGraph(), graph1 = HReassertModel.createGraph();
		final Artifact<MockCoordinates> a0 = new Artifact<>(null, new MockCoordinates("a0")), a1 = new Artifact<>(null, new MockCoordinates("a1"));
		graph0.addVertex(a0);
		graph0.addVertex(a1);
		graph0.addEdge(a0, a1, new Copy());

		graph1.addVertex(a1);
		graph1.addVertex(a0);
		graph1.addEdge(a1, a0, new Copy());

		HAssert.assertTrue(new ReassertGraphIsomorphism(graph0, graph1).isMatch());
	}

	@Test
	public void inheritsDirected() {
		final Graph<IVertex, IEdge> graph0 = HReassertModel.createGraph(), graph1 = HReassertModel.createGraph();
		final Artifact<MockCoordinates> a0 = new Artifact<>(null, new MockCoordinates("a0")), a1 = new Artifact<>(null, new MockCoordinates("a1"));
		graph0.addVertex(a0);
		graph0.addVertex(a1);
		graph0.addEdge(a0, a1, new Inherits());

		graph1.addVertex(a1);
		graph1.addVertex(a0);
		graph1.addEdge(a1, a0, new Inherits());

		HAssert.assertFalse(new ReassertGraphIsomorphism(graph0, graph1).isMatch());
	}
}
