package com.g2forge.reassert.reassert.algorithm;

import org.jgrapht.Graph;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.list.ListRepository;
import com.g2forge.reassert.reassert.ReassertContext;
import com.g2forge.reassert.reassert.algorithm.example.ExampleGraph;
import com.g2forge.reassert.reassert.convert.finding.FindingModule;
import com.g2forge.reassert.reassert.convert.license.StandardLicenseModule;
import com.g2forge.reassert.reassert.convert.term.StandardTermModule;
import com.g2forge.reassert.reassert.convert.work.WorkModule;

import lombok.Getter;

public abstract class ATestVisitor {
	@Getter(lazy = true)
	private final ListRepository repository = computeRepository();

	public ListRepository computeRepository() {
		final IContext context = ReassertContext.getContext();
		final ReassertVertexDescriber vertexDescriber = new ReassertVertexDescriber(context);
		final ListRepository repository = new ListRepository(context, vertexDescriber) {
			protected ObjectMapper computeMapper() {
				final ObjectMapper mapper = super.computeMapper();
				mapper.registerModule(new StandardLicenseModule());
				mapper.registerModule(new StandardTermModule());
				mapper.registerModule(new FindingModule(vertexDescriber));
				mapper.registerModule(new WorkModule());
				return mapper;
			}
		};
		return repository;
	}

	protected abstract ExampleGraph load(final Artifact<ListCoordinates> artifact);

	protected ExampleGraph load(String name) {
		return load(new Artifact<>(getRepository(), new ListCoordinates(new ResourceDataSource(new Resource(getClass(), name + "-input.json")))));
	}

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
