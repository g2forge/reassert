package com.g2forge.reassert.list;

import org.jgrapht.Graph;
import org.junit.Test;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.enigma.diagram.dot.convert.DotRenderer;
import com.g2forge.reassert.core.algorithm.ReassertGraphVisualizer;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.test.ATestRepository;
import com.g2forge.reassert.mock.MockCoordinates;

public class TestListRepository extends ATestRepository<ListCoordinates> {
	@Override
	protected ListCoordinates createCoordinates() {
		return new ListCoordinates(new ResourceDataSource(new Resource(getClass(), "test.json")));
	}

	@Override
	protected ListRepository getRepository() {
		final IContext context = getContext();
		return new ListRepository(context, new ReassertVertexDescriber(context));
	}

	@Override
	protected ISystem<ListCoordinates> getSystem() {
		return ListSystem.create();
	}

	@Test
	public void loadstore() {
		final ListRepository reposiory = getRepository();
		final Resource resource = new Resource(getClass(), "test.json");

		final Graph<IVertex, IEdge> graph = reposiory.load(new ResourceDataSource(resource));

		final ByteArrayDataSink sink = new ByteArrayDataSink();
		reposiory.store(graph, sink);

		HAssert.assertEquals(resource.read(false), sink.getStream().toString());
	}

	@Test
	public void test() {
		final Graph<IVertex, IEdge> graph = getGraph();
		final MockCoordinates coordinates = HCollection.getOne(HReassertModel.findCoordinates(graph, new MockCoordinates("ax-java")));
		HAssert.assertInstanceOf(Artifact.class, graph.getEdgeSource(HCollection.getOne(graph.incomingEdgesOf(coordinates))));
	}

	@Test
	public void visualize() {
		final String actual = new DotRenderer().render(new ReassertGraphVisualizer(getContext()).visualize(getGraph()));
		HAssert.assertEquals(new Resource(getClass(), "test.dot"), actual);
	}
}
