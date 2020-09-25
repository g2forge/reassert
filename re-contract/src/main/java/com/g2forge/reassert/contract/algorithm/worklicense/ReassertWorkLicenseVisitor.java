package com.g2forge.reassert.contract.algorithm.worklicense;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.jgrapht.Graph;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.core.error.UnreachableCodeError;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.contract.ContractComparisonAnalyzer;
import com.g2forge.reassert.contract.algorithm.worklicense.model.finding.UnknownWorkLicenseRulesFinding;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRules;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRulesFactory;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRulesFactoryFactory;
import com.g2forge.reassert.core.algorithm.visitor.AGraphVisitor;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.core.model.work.Work;
import com.g2forge.reassert.core.model.work.WorkLicense;
import com.g2forge.reassert.core.model.work.WorkMember;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReassertWorkLicenseVisitor extends AGraphVisitor {
	protected final AtomicInteger workSequence = new AtomicInteger(0);

	protected final IWorkLicenseRulesFactoryFactory rulesFactoryFactory;

	@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
	@Override
	public void accept(Graph<IVertex, IEdge> graph) {
		final FindingConsumer findingConsumer = new FindingConsumer(graph);

		final Map<ILicenseApplied, IWorkLicenseRulesFactory> licenses = new LinkedHashMap<>();
		final IWorkLicenseRulesFactoryFactory rulesFactoryFactory = getRulesFactoryFactory();
		for (ILicenseApplied license : graph.vertexSet().stream().flatMap(ITypeRef.of(ILicenseApplied.class)::castIfInstance).collect(Collectors.toCollection(LinkedHashSet::new))) {
			final IWorkLicenseRulesFactory factory;
			try {
				factory = rulesFactoryFactory.apply(license);
			} catch (Throwable throwable) {
				findingConsumer.found(new UnknownWorkLicenseRulesFinding(throwable), license);
				continue;
			}
			if (factory != null) licenses.put(license, factory);
		}

		final LinkedHashSet<WorkMember> queue = new LinkedHashSet<>();
		// Start with all the vertices which have licenses assigned
		for (ILicenseApplied license : licenses.keySet()) {
			for (Artifact<?> artifact : HReassertModel.get(graph, license, false, Notice.class::isInstance, new ATypeRef<Artifact<?>>() {})) {
				final int index = workSequence.getAndIncrement();
				final String name = "Work " + index;
				final Work work = new Work(name);
				if (!name.equals(work.getName())) workSequence.compareAndSet(index + 1, index);

				graph.addVertex(work);
				graph.addEdge(work, license, new WorkLicense());
				final WorkMember member = new WorkMember();
				graph.addEdge(artifact, work, member);
				queue.add(member);
			}
		}

		// For each artifact added to a work (represented by an edge)...
		while (!queue.isEmpty()) {
			final WorkMember member;
			{ // Get the first artifact on the queue
				final Iterator<WorkMember> iterator = queue.iterator();
				member = iterator.next();
				iterator.remove();
			}

			final Work work = (Work) graph.getEdgeTarget(member);
			final IWorkLicenseRulesFactory rulesFactory = licenses.get(getLicense(graph, work));
			final Artifact<?> artifact = (Artifact<?>) graph.getEdgeSource(member);

			final Stream<Artifact<?>> incoming = graph.incomingEdgesOf(artifact).stream().filter(e -> graph.getEdgeSource(e) instanceof Artifact).filter(e -> rulesFactory.isIncluded(e, false)).map(graph::getEdgeSource).flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance);
			final Stream<Artifact<?>> outgoing = graph.outgoingEdgesOf(artifact).stream().filter(e -> graph.getEdgeTarget(e) instanceof Artifact).filter(e -> rulesFactory.isIncluded(e, true)).map(graph::getEdgeTarget).flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance);
			final Set<Artifact<?>> artifacts = HStream.concat(incoming, outgoing).filter(a -> graph.getEdge(a, work) == null).collect(Collectors.toCollection(LinkedHashSet::new));

			for (Artifact<?> otherArtifact : artifacts) {
				final WorkMember otherMember = new WorkMember();
				graph.addEdge(otherArtifact, work, otherMember);
				queue.add(otherMember);
			}
		}

		for (Work work : graph.vertexSet().stream().flatMap(ITypeRef.of(Work.class)::castIfInstance).collect(Collectors.toList())) {
			final ILicenseApplied workLicense = getLicense(graph, work);
			final IWorkLicenseRulesFactory rulesFactory = licenses.get(workLicense);

			final Collection<Artifact<?>> artifacts = HReassertModel.get(graph, work, false, WorkMember.class::isInstance, new ATypeRef<Artifact<?>>() {});
			final Map<ILicenseApplied, List<Artifact<?>>> combinedLinceses = artifacts.stream().collect(HCollector.multiGroupingBy(artifact -> {
				final Collection<ILicenseApplied> retVal = HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(ILicenseApplied.class));
				if (isLicenseRequired() && retVal.isEmpty()) throw new UnreachableCodeError("All artifacts require a license (at least " + UnspecifiedLicense.create().getShortID() + ") before work license analysis, but " + artifact.getCoordinates() + " did not have a license!");
				return retVal;
			}));
			for (Map.Entry<ILicenseApplied, List<Artifact<?>>> entry : combinedLinceses.entrySet()) {
				final ILicenseFamily combinedLicense = (ILicenseFamily) entry.getKey();

				final IWorkLicenseRules rules = rulesFactory.apply(combinedLicense);
				if (rules == null) continue;
				final ContractComparisonAnalyzer analyzer = new ContractComparisonAnalyzer(rules);
				analyzer.analyze(workLicense, combinedLicense, new FindingConsumer(graph, work, workLicense, combinedLicense));
			}
		}
	}

	protected ILicenseApplied getLicense(Graph<IVertex, IEdge> graph, final Work work) {
		return HCollection.getOne(HReassertModel.get(graph, work, true, WorkLicense.class::isInstance, ITypeRef.of(ILicenseApplied.class)));
	}

	/**
	 * Check if licenses must be specified for all artifacts. This method exists to facilitate testing.
	 * 
	 * @return {@code true} if licenses must be specified for all artifacts.
	 */
	protected boolean isLicenseRequired() {
		return true;
	}
}
