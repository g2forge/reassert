package com.g2forge.reassert.core.algorithm.visitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.shortestpath.AllDirectedPaths;
import org.jgrapht.graph.EdgeReversedGraph;

import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.error.OrThrowable;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.workqueue.BasicWorkQueueFactory;
import com.g2forge.alexandria.workqueue.IWorkQueueContext;
import com.g2forge.alexandria.workqueue.IWorkQueueHandler;
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
import com.g2forge.reassert.core.model.contract.IContractApplied;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.report.IOrigins;

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
	protected static class ArtifactLoadContext {
		protected final Graph<IVertex, IEdge> graph;

		@Getter(lazy = true)
		private final Graph<Artifact<?>, IEdge> artifactGraph = new EdgeReversedGraph<>(HReassertModel.asArtifactGraph(graph));

		@Getter(lazy = true)
		private final AllDirectedPaths<Artifact<?>, IEdge> paths = new AllDirectedPaths<>(getArtifactGraph());

		@Getter(lazy = true)
		private final Set<Artifact<?>> roots = Collections.unmodifiableSet(getArtifactGraph().vertexSet().stream().filter(v -> getArtifactGraph().outDegreeOf(v) == 0).collect(Collectors.toSet()));

		protected final Map<ICoordinates, OrThrowable<String>> coordinatesToPath = new HashMap<>();

		protected <Coordinates extends ICoordinates> IDescription describe(Artifact<Coordinates> a) {
			return a.getRepository().getSystem().getCoordinateDescriber().describe(a.getCoordinates());
		}

		public OrThrowable<String> getPaths(ICoordinates coordinates) {
			return coordinatesToPath.computeIfAbsent(coordinates, c -> {
				try {
					final Artifact<ICoordinates> found = HReassertModel.findArtifact(getGraph(), c);

					final Set<Artifact<?>> targets = new HashSet<>(getRoots());
					targets.remove(found);
					final List<GraphPath<Artifact<?>, IEdge>> pathList = getPaths().getAllPaths(HCollection.asSet(found), targets, true, Integer.MAX_VALUE);
					return new OrThrowable<String>(pathList.stream().map(GraphPath::getVertexList).map((List<Artifact<?>> al) -> {
						final List<Artifact<?>> reversed = new ArrayList<>(al);
						Collections.reverse(reversed);
						return reversed.stream().map(a -> describe(a).getName()).collect(Collectors.joining(" -> "));
					}).collect(Collectors.joining("\n")));
				} catch (Throwable throwable) {
					return new OrThrowable<>(throwable);
				}
			});
		}
	}

	protected static class ArtifactLoadException extends RuntimeException {
		private static final long serialVersionUID = 5809831159823219505L;

		protected final ICoordinates coordinates;

		protected final IRepository<? extends ICoordinates> repository;

		protected final ArtifactLoadContext context;

		@Getter(lazy = true)
		private final String message = computeMessage();

		public <Coordinates extends ICoordinates> ArtifactLoadException(Coordinates coordinates, IRepository<Coordinates> repository, ArtifactLoadContext context, Throwable cause) {
			super(null, cause);
			this.coordinates = coordinates;
			this.repository = repository;
			this.context = context;
		}

		protected String computeMessage() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			final String coordinateString = describe(coordinates, (IRepository) repository);

			log.info(String.format("Computing exception message for %1$s", coordinateString));
			final OrThrowable<String> paths = context.getPaths(coordinates);
			return String.format("Failed to load artifact for coordinates %1$s\n%2$s", coordinateString, paths);
		}

		protected <Coordinates extends ICoordinates> String describe(Coordinates coordinates, IRepository<Coordinates> repository) {
			try {
				return repository.getSystem().getCoordinateDescriber().describe(coordinates).getName();
			} catch (Throwable throwable) {
				addSuppressed(throwable);
				return coordinates.toString();
			}
		}
	}

	@Getter
	@RequiredArgsConstructor
	@ToString
	protected static class Callback<Coordinates extends ICoordinates> implements IReassertGraphBuilder.ICallback<Coordinates> {
		protected final Artifact<Coordinates> artifact;

		protected final List<IContractApplied> notices;

		@Override
		public void callback(Artifact<Coordinates> artifact, IReassertGraphBuilder builder) {
			if (getNotices() != null) for (IContractApplied notice : getNotices()) {
				builder.vertex(notice);
				builder.edge(artifact, notice, new Notice());
			}
		}
	}

	protected final IContext context;

	public Graph<IVertex, IEdge> createGraph(IOrigins origins) {
		final Graph<IVertex, IEdge> graph = HReassertModel.createGraph();
		for (Map.Entry<Artifact<?>, List<IContractApplied>> origin : origins.getOrigins().entrySet()) {
			extendGraph(new Callback<>(origin.getKey(), origin.getValue()), graph);
		}
		return graph;
	}

	protected void extendGraph(final Callback<?> callback, final Graph<IVertex, IEdge> graph) {
		final ArtifactLoadContext artifactLoadContext = new ArtifactLoadContext(graph);
		final Collection<Throwable> failures = new ArrayList<>();
		BasicWorkQueueFactory.create().run(new IWorkQueueHandler<IReassertGraphBuilder.ICallback<?>>() {
			@Override
			public void run(IWorkQueueContext<IReassertGraphBuilder.ICallback<?>> context, IReassertGraphBuilder.ICallback<?> task) {
				handle(artifactLoadContext, context, task, failures);
			}
		}, callback);
		if (!failures.isEmpty()) throw HError.withSuppressed(new RuntimeException("Failed to load one or more artifacts!"), failures);
	}

	protected <Coordinates extends ICoordinates> void handle(final ArtifactLoadContext loadContext, final IWorkQueueContext<IReassertGraphBuilder.ICallback<?>> queueContext, final ICallback<Coordinates> callback, final Collection<Throwable> failures) {
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

			log.info(String.format("Starting %1$s with %2$d remaining", name, queueContext.getApproximateQueueSize()));
		}

		final ReassertGraphBuilder builder = new ReassertGraphBuilder(loadContext.getGraph(), callbacks);
		final Artifact<Coordinates> artifact;
		try {
			artifact = repository.load(coordinates, builder);
		} catch (Throwable throwable) {
			final ArtifactLoadException exception = new ArtifactLoadException(coordinates, repository, loadContext, throwable);
			try {
				final Artifact<Coordinates> found = HReassertModel.findArtifact(loadContext.getGraph(), coordinates);
				callback.callback(found, builder);
			} catch (Throwable nested) {
				exception.addSuppressed(nested);
			}
			failures.add(exception);
			return;
		}
		callback.callback(artifact, builder);

		queueContext.queue(callbacks);
	}
}
