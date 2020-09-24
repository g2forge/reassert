package com.g2forge.reassert.contract.algorithm.work;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.contract.algorithm.work.model.finding.UnknownWorkFinding;
import com.g2forge.reassert.contract.algorithm.work.model.rule.IWorkRule;
import com.g2forge.reassert.contract.algorithm.work.model.rule.IWorkRules;
import com.g2forge.reassert.contract.algorithm.work.model.rule.RuleWorkType;
import com.g2forge.reassert.core.algorithm.visitor.AGraphVisitor;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.work.Work;
import com.g2forge.reassert.core.model.work.WorkContains;
import com.g2forge.reassert.core.model.work.WorkLicense;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReassertWorkVisitor extends AGraphVisitor {
	protected final AtomicInteger workSequence = new AtomicInteger(0);

	protected final IWorkRules workTypeFactory;

	@Override
	public void accept(Graph<IVertex, IEdge> graph) {
		createWorks(graph);
		analyzeWorks(graph);
	}

	protected void analyzeWorks(Graph<IVertex, IEdge> graph) {
		for (Work work : graph.vertexSet().stream().flatMap(ITypeRef.of(Work.class)::castIfInstance).collect(Collectors.toList())) {
			getRule(work).analyze(graph, work, new FindingConsumer(graph, work));
		}
	}

	protected void createWorks(Graph<IVertex, IEdge> graph) {
		final FindingConsumer findingConsumer = new FindingConsumer(graph);

		final Map<ILicenseApplied, RuleWorkType> licenses = new LinkedHashMap<>();
		final IWorkRules workTypeFactory = getWorkTypeFactory();
		for (ILicenseApplied license : graph.vertexSet().stream().flatMap(ITypeRef.of(ILicenseApplied.class)::castIfInstance).collect(Collectors.toCollection(LinkedHashSet::new))) {
			final RuleWorkType workType;
			try {
				workType = workTypeFactory.apply(license);
			} catch (Throwable throwable) {
				findingConsumer.found(new UnknownWorkFinding(throwable), license);
				continue;
			}
			if (workType != null) licenses.put(license, workType);
		}
		final LinkedHashSet<WorkContains> queue = new LinkedHashSet<>();

		// Start with all the vertices which have licenses assigned
		for (Map.Entry<ILicenseApplied, ? extends RuleWorkType> entry : licenses.entrySet()) {
			final RuleWorkType workType = entry.getValue();
			for (Artifact<?> artifact : HReassertModel.get(graph, entry.getKey(), false, Notice.class::isInstance, new ATypeRef<Artifact<?>>() {})) {
				final int index = workSequence.getAndIncrement();
				final String name = "Work " + index;
				final Work work = new Work(name, workType);
				if (!name.equals(work.getName())) workSequence.compareAndSet(index + 1, index);

				graph.addVertex(work);
				graph.addEdge(work, entry.getKey(), new WorkLicense());
				final WorkContains contains = new WorkContains();
				graph.addEdge(work, artifact, contains);
				queue.add(contains);
			}
		}

		// For each artifact added to a work (represented by an edge)...
		while (!queue.isEmpty()) {
			final WorkContains contains;
			{ // Get the first artifact on the queue
				final Iterator<WorkContains> iterator = queue.iterator();
				contains = iterator.next();
				iterator.remove();
			}

			final Work work = (Work) graph.getEdgeSource(contains);
			final Artifact<?> artifact = (Artifact<?>) graph.getEdgeTarget(contains);

			final Stream<Artifact<?>> incoming = graph.incomingEdgesOf(artifact).stream().filter(e -> graph.getEdgeSource(e) instanceof Artifact).filter(e -> getRule(work).isIncluded(e, false)).map(graph::getEdgeSource).flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance);
			final Stream<Artifact<?>> outgoing = graph.outgoingEdgesOf(artifact).stream().filter(e -> graph.getEdgeTarget(e) instanceof Artifact).filter(e -> getRule(work).isIncluded(e, true)).map(graph::getEdgeTarget).flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance);
			final Set<Artifact<?>> artifacts = HStream.concat(incoming, outgoing).filter(a -> graph.getEdge(work, a) == null).collect(Collectors.toCollection(LinkedHashSet::new));

			for (Artifact<?> otherArtifact : artifacts) {
				final WorkContains otherContains = new WorkContains();
				graph.addEdge(work, otherArtifact, otherContains);
				queue.add(otherContains);
			}
		}
	}

	protected IWorkRule getRule(final Work work) {
		return ((RuleWorkType) work.getType()).getRule();
	}
}
