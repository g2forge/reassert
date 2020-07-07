package com.g2forge.reassert.reassert.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.shortestpath.AllDirectedPaths;
import org.jgrapht.graph.EdgeReversedGraph;

import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.api.IReassertGraphBuilder.ICallback;
import com.g2forge.reassert.core.api.ReassertGraphBuilder;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.system.IRepository;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.reassert.Origins;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
@Slf4j
public class ReassertGraphExplorer {
	@Getter
	@RequiredArgsConstructor
	@ToString
	protected static class Callback<Coordinates extends ICoordinates> implements IReassertGraphBuilder.ICallback<Coordinates> {
		protected final Artifact<Coordinates> artifact;

		@Override
		public void callback(Artifact<Coordinates> artifact, IReassertGraphBuilder builder) {}
	}

	protected final IContext context;

	public Graph<IVertex, IEdge> createGraph(Origins origins) {
		final Graph<IVertex, IEdge> graph = HReassertModel.createGraph();
		for (Artifact<?> origin : origins.getOrigins()) {
			extendGraph(origin, graph);
		}
		return graph;
	}

	protected <Coordinates extends ICoordinates> IDescription describe(Artifact<Coordinates> a) {
		return a.getRepository().getSystem().getCoordinateDescriber().describe(a.getCoordinates());
	}

	protected void extendGraph(final Artifact<?> origin, final Graph<IVertex, IEdge> graph) {
		// Don't bother if we already explored this origin
		if (graph.vertexSet().contains(origin)) return;

		final LinkedList<IReassertGraphBuilder.ICallback<?>> queue = new LinkedList<>();
		final Collection<Throwable> failures = new ArrayList<>();
		queue.add(new Callback<>(origin));
		while (!queue.isEmpty()) {
			handle(graph, queue, queue.remove(), failures);
		}
		if (!failures.isEmpty()) throw HError.multithrow("Failed to load one or more artifacts!", failures);
	}

	protected <Coordinates extends ICoordinates> void handle(final Graph<IVertex, IEdge> graph, final Collection<IReassertGraphBuilder.ICallback<?>> queue, final ICallback<Coordinates> callback, final Collection<Throwable> failures) {
		final List<IReassertGraphBuilder.ICallback<?>> callbacks = new ArrayList<>();

		final Artifact<Coordinates> unloadedArtifact = callback.getArtifact();
		final Coordinates unloadedCoordinates = unloadedArtifact.getCoordinates(), coordinates;

		if (unloadedCoordinates.getSystem() != null) coordinates = unloadedCoordinates;
		else {
			final ISystem<Coordinates> system = context.findSystem(unloadedCoordinates);
			coordinates = system.withSystem(unloadedCoordinates);
		}

		final IRepository<Coordinates> repository;
		if (unloadedArtifact.getRepository() != null) repository = unloadedArtifact.getRepository();
		else {
			@SuppressWarnings("unchecked")
			final ISystem<Coordinates> system = (ISystem<Coordinates>) coordinates.getSystem();
			repository = system.getRepository();
		}

		if (log.isInfoEnabled()) {
			String name;
			try {
				final IDescriber<Coordinates> describer = repository.getSystem().getCoordinateDescriber();
				name = describer.describe(coordinates).getName();
			} catch (Throwable throwable) {
				name = coordinates.toString();
			}

			log.info(String.format("Starting %1$s with %2$d remaining", name, queue.size()));
		}

		final Artifact<Coordinates> artifact;
		try {
			artifact = repository.load(coordinates, new ReassertGraphBuilder(graph, callbacks));
		} catch (Throwable throwable) {
			// Print a really good exception, one which helps us know the artifact that couldn't be loaded and where it's being pulled in
			String paths = "";
			final List<Throwable> suppressed = new ArrayList<>();;
			try {
				final Artifact<Coordinates> found = HReassertModel.findArtifact(graph, coordinates);
				callback.callback(found, new ReassertGraphBuilder(graph, callbacks));

				final EdgeReversedGraph<Artifact<?>, IEdge> artifactGraph = new EdgeReversedGraph<>(HReassertModel.asArtifactGraph(graph));
				final Set<Artifact<?>> targets = artifactGraph.vertexSet().stream().filter(v -> artifactGraph.outDegreeOf(v) == 0).collect(Collectors.toSet());
				targets.remove(found);
				final List<GraphPath<Artifact<?>, IEdge>> pathList = new AllDirectedPaths<>(artifactGraph).getAllPaths(HCollection.asSet(found), targets, true, Integer.MAX_VALUE);
				paths = "\n" + pathList.stream().map(GraphPath::getVertexList).map((List<Artifact<?>> al) -> {
					final List<Artifact<?>> reversed = new ArrayList<>(al);
					Collections.reverse(reversed);
					return reversed.stream().map(a -> describe(a).getName()).collect(Collectors.joining(" -> "));
				}).collect(Collectors.joining("\n"));
			} catch (Throwable nested) {
				suppressed.add(nested);
			}

			String coordinateString;
			try {
				coordinateString = repository.getSystem().getCoordinateDescriber().describe(coordinates).getName();
			} catch (Throwable t) {
				coordinateString = coordinates.toString();
				suppressed.add(t);
			}
			final RuntimeException toThrow = new RuntimeException(String.format("Failed to load artifact for coordinates %1$s%2$s", coordinateString, paths), throwable);
			suppressed.forEach(toThrow::addSuppressed);
			failures.add(toThrow);
			return;
		}
		callback.callback(artifact, new ReassertGraphBuilder(graph, callbacks));

		queue.addAll(callbacks);
	}
}
