package com.g2forge.reassert.list;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.jgrapht.Graph;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.adt.identity.Identified;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSink;
import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.api.ReassertGraphBuilder;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.system.ARepository;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.convert.GraphModule;
import com.g2forge.reassert.list.convert.TermsModule;
import com.g2forge.reassert.list.convert.simpleedge.SimpleEdgeModule;
import com.g2forge.reassert.list.model.EdgeIdentity;
import com.g2forge.reassert.list.model.StoredEdge;
import com.g2forge.reassert.list.model.StoredGraph;
import com.g2forge.reassert.list.model.StoredVertex;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class ListRepository extends ARepository<ListCoordinates, ListSystem> {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ObjectMapper mapper = computeMapper();

	protected final IContext context;

	protected ObjectMapper computeMapper() {
		final ObjectMapper mapper = new ObjectMapper();
		mapper.enable(SerializationFeature.INDENT_OUTPUT);
		mapper.registerModule(new ParanamerModule());
		mapper.registerModule(new SimpleEdgeModule());
		mapper.registerModule(new TermsModule());
		mapper.registerModule(new GraphModule(getContext()));
		return mapper;
	}

	@Override
	public ListSystem getSystem() {
		return ListSystem.create();
	}

	public Graph<IVertex, IEdge> load(IDataSource source) {
		final Graph<IVertex, IEdge> retVal = HReassertModel.createGraph();
		load(source, new ReassertGraphBuilder(retVal, null));
		return retVal;
	}

	protected void load(IDataSource source, final IReassertGraphBuilder graphBuilder) {
		final StoredGraph storedGraph;
		try (final InputStream input = source.getStream(ITypeRef.of(InputStream.class))) {
			final ObjectMapper mapper = getMapper();
			storedGraph = mapper.readValue(input, StoredGraph.class);
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}

		final Map<String, StoredVertex> vertices = storedGraph.getVertices();
		for (StoredVertex vertex : vertices.values()) {
			graphBuilder.vertex(vertex.getVertex());
		}

		for (StoredVertex vertex : vertices.values()) {
			if (vertex.getOutgoings() != null) for (StoredEdge edge : vertex.getOutgoings()) {
				for (String target : edge.getTargets()) {
					final IEdge cloned = edge.getEdge().clone();
					graphBuilder.edge(vertex.getVertex(), vertices.get(target).getVertex(), cloned);
				}
			}
		}
	}

	@Override
	public Artifact<ListCoordinates> load(ListCoordinates coordinates, IReassertGraphBuilder builder) {
		assertValid(coordinates);
		this.load(coordinates.getSource(), builder);
		return null;
	}

	public void store(Graph<IVertex, IEdge> graph, IDataSink sink) {
		final StoredGraph.StoredGraphBuilder storedGraphBuilder = StoredGraph.builder();
		for (IVertex vertex : graph.vertexSet()) {
			final StoredVertex.StoredVertexBuilder storedVertexBuilder = StoredVertex.builder().vertex(vertex);

			final Set<IEdge> edges = graph.outgoingEdgesOf(vertex);
			if ((edges != null) && !edges.isEmpty()) {
				final Map<Identified<IEdge>, List<IEdge>> grouped = edges.stream().collect(Collectors.groupingBy(e -> new Identified<>(e, EdgeIdentity.create())));

				final List<Identified<IEdge>> keys = new ArrayList<>(grouped.keySet());
				keys.sort(new Comparator<Identified<IEdge>>() {
					@Override
					public int compare(Identified<IEdge> o1, Identified<IEdge> o2) {
						return o1.get().getClass().getSimpleName().compareTo(o2.get().getClass().getSimpleName());
					}
				});

				for (Identified<IEdge> key : keys) {
					final List<String> targets = grouped.get(key).stream().map(graph::getEdgeTarget).map(context::describe).map(IDescription::getIdentifier).collect(Collectors.toList());
					storedVertexBuilder.outgoing(new StoredEdge(key.get(), targets));
				}
			}

			final String identifier = context.describe(vertex).getIdentifier();
			if (identifier == null) { throw new NullPointerException(String.format("Null identifier for vertex: %1$s", vertex)); }
			storedGraphBuilder.vertex(identifier, storedVertexBuilder.build());
		}
		final StoredGraph storedGraph = storedGraphBuilder.build();

		final ObjectMapper mapper = getMapper();
		try (final OutputStream output = sink.getStream(ITypeRef.of(OutputStream.class));) {
			mapper.writeValue(output, storedGraph);
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
	}
}
