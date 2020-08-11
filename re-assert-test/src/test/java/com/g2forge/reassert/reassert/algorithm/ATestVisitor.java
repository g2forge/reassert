package com.g2forge.reassert.reassert.algorithm;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.reassert.ATestReassert;
import com.g2forge.reassert.reassert.algorithm.example.ExampleGraph;

public abstract class ATestVisitor extends ATestReassert {
	protected void test(String name) {
		test(name, null);
	}

	protected void test(String name, IFunction1<? super Graph<IVertex, IEdge>, ? extends Graph<IVertex, IEdge>> prestore) {
		final ExampleGraph exampleGraph = load(name);

		final ByteArrayDataSink sink = new ByteArrayDataSink();
		final Graph<IVertex, IEdge> graph = prestore == null ? exampleGraph.getGraph() : prestore.apply(exampleGraph.getGraph());
		getRepository().store(graph, sink);

		HAssert.assertEquals(new Resource(getClass(), name + "-output.json").read(false), sink.getStream().toString());
	}
}
