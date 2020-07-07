package com.g2forge.reassert.core.model;

import java.util.Collection;
import java.util.stream.Collectors;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.MaskSubgraph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.marker.Helpers;
import com.g2forge.alexandria.java.function.IPredicate1;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.coordinates.Coordinates;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.experimental.UtilityClass;

@UtilityClass
@Helpers
public class HReassertModel {
	public static Graph<IVertex, IEdge> clone(final Graph<IVertex, IEdge> graph) {
		if (graph instanceof DefaultDirectedGraph) {
			@SuppressWarnings("unchecked")
			final Graph<IVertex, IEdge> retVal = (Graph<IVertex, IEdge>) ((DefaultDirectedGraph<IVertex, IEdge>) graph).clone();
			return retVal;
		}

		final Graph<IVertex, IEdge> retVal = createGraph();
		graph.vertexSet().forEach(retVal::addVertex);
		for (IEdge edge : graph.edgeSet()) {
			retVal.addEdge(graph.getEdgeSource(edge), graph.getEdgeTarget(edge), edge.clone());
		}
		return retVal;
	}

	public static Graph<IVertex, IEdge> asLicenseGraph(Graph<IVertex, IEdge> graph) {
		return new MaskSubgraph<>(graph, v -> !((v instanceof Artifact) || (v instanceof ILicense)), e -> false);
	}

	public static Graph<Artifact<?>, IEdge> asArtifactGraph(Graph<IVertex, IEdge> graph) {
		@SuppressWarnings({ "rawtypes", "unchecked" })
		final Graph<Artifact<?>, IEdge> retVal = (Graph) new MaskSubgraph<>(graph, v -> !(v instanceof Artifact), e -> false);
		return retVal;
	}

	public static Graph<IVertex, IEdge> createGraph() {
		return new DefaultDirectedGraph<>(null, null, false);
	}

	public <C extends ICoordinates> Collection<C> findCoordinates(Graph<IVertex, IEdge> graph, C coordinates) {
		@SuppressWarnings("unchecked")
		final ITypeRef<C> type = ITypeRef.of((Class<C>) coordinates.getClass());
		return graph.vertexSet().stream().flatMap(type::castIfInstance).filter(c -> c.equals(coordinates)).collect(Collectors.toList());
	}

	public <C extends ICoordinates> Artifact<C> findArtifact(Graph<IVertex, IEdge> graph, C coordinates) {
		final C found = HCollection.getOne(findCoordinates(graph, coordinates));
		final Collection<Artifact<C>> artifacts = get(graph, found, false, Coordinates.class::isInstance, new ATypeRef<Artifact<C>>() {});
		return HCollection.getOne(artifacts);
	}

	public <T> Collection<T> get(Graph<IVertex, IEdge> graph, IVertex vertex, boolean outgoing, IPredicate1<? super IEdge> edgePredicate, ITypeRef<T> vertexType) {
		final Collection<IEdge> edges = outgoing ? graph.outgoingEdgesOf(vertex) : graph.incomingEdgesOf(vertex);
		return edges.stream().filter(edgePredicate).map(outgoing ? graph::getEdgeTarget : graph::getEdgeSource).flatMap(vertexType::castIfInstance).collect(Collectors.toList());
	}

	public Collection<IEdge> getEdges(Graph<IVertex, IEdge> graph, IVertex vertex, boolean outgoing, IPredicate1<? super IEdge> edgePredicate, ITypeRef<?> vertexType) {
		final Collection<IEdge> edges = outgoing ? graph.outgoingEdgesOf(vertex) : graph.incomingEdgesOf(vertex);
		return edges.stream().filter(edgePredicate).filter(edge -> vertexType.isInstance(outgoing ? graph.getEdgeTarget(edge) : graph.getEdgeSource(edge))).collect(Collectors.toList());
	}
}
