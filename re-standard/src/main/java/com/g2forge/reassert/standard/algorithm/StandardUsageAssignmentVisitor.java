package com.g2forge.reassert.standard.algorithm;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.algorithm.visitor.AGraphVisitor;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.MergedUsage;
import com.g2forge.reassert.core.model.contract.usage.UnspecifiedUsage;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class StandardUsageAssignmentVisitor extends AGraphVisitor {
	protected final IFunction2<? super IEdge, ? super IUsageApplied, ? extends IUsage> propagate;

	@Override
	public void accept(Graph<IVertex, IEdge> graph) {
		final UnspecifiedUsage unspecifiedUsage = UnspecifiedUsage.create();
		graph.addVertex(unspecifiedUsage);

		final LinkedHashSet<Artifact<?>> artifacts = graph.vertexSet().stream().flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance).collect(Collectors.toCollection(LinkedHashSet::new));
		final LinkedHashSet<Artifact<?>> queue = new LinkedHashSet<Artifact<?>>();

		// Make sure all artifacts have a usage specified (even if it's unknown) and queue up any artifacts that have a real usage
		for (Artifact<?> artifact : artifacts) {
			final Collection<IUsageApplied> usages = HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(IUsageApplied.class));
			if (usages.isEmpty()) graph.addEdge(artifact, unspecifiedUsage, new Notice());
			else queue.addAll(HReassertModel.get(graph, artifact, true, IEdge::isDirected, new ATypeRef<Artifact<?>>() {}));
		}

		// While there's an artifact in need of updating
		while (!queue.isEmpty()) {
			final Artifact<?> artifact;
			{ // Get the first artifact on the queue
				final Iterator<Artifact<?>> iterator = queue.iterator();
				artifact = iterator.next();
				iterator.remove();
			}

			final Collection<IEdge> usageEdges = HReassertModel.getEdges(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(IUsageApplied.class));
			final Collection<IEdge> incomingEdges = HReassertModel.getEdges(graph, artifact, false, IEdge::isDirected, new ATypeRef<Artifact<?>>() {});

			// Compute the original usages, and create a collection for the new ones to merge
			final Collection<IUsage> originalUsages = usageEdges.stream().map(graph::getEdgeTarget).map(IUsage.class::cast).collect(Collectors.toCollection(LinkedHashSet::new));
			final Set<IUsage> usages = new LinkedHashSet<>(originalUsages);

			// Find all the usages which could affect this vertex
			for (IEdge edge : incomingEdges) {
				final IVertex otherArtifact = graph.getEdgeSource(edge);
				final Collection<IUsageApplied> otherUsages = HReassertModel.get(graph, otherArtifact, true, Notice.class::isInstance, ITypeRef.of(IUsageApplied.class));
				for (IUsageApplied otherUsage : otherUsages) {
					final IUsage modified = propagate(edge, otherUsage);
					if ((modified != null) && !unspecifiedUsage.equals(modified)) usages.add(modified);
				}
			}

			// If nothing can have changed, and we don't need to merge multiple initial usages, then continue
			if (usages.equals(originalUsages) && (originalUsages.size() == 1)) continue;
			final IUsage merged = merge(usages);
			// If nothing actual changed after merging, then continue;
			if ((originalUsages.size() == 1) && HCollection.getOne(originalUsages).getTerms().equals(merged.getTerms())) continue;

			// Update the usage for artifact
			for (IEdge edge : usageEdges) {
				final IVertex target = graph.getEdgeTarget(edge);
				graph.removeEdge(edge);
				if (graph.degreeOf(target) == 0) graph.removeVertex(target);
			}
			graph.addVertex(merged);
			graph.addEdge(artifact, merged, new Notice());

			// Might need to update everything this can affect
			queue.addAll(HReassertModel.get(graph, artifact, true, IEdge::isDirected, new ATypeRef<Artifact<?>>() {}));
		}
	}

	@ReassertLegalOpinion
	protected IUsage merge(Set<IUsage> usages) {
		final MergedUsage merged = new MergedUsage(usages);
		for (IUsage usage : usages) {
			if (usage.getTerms().equals(merged.getTerms())) return usage;
		}
		return merged;
	}

	protected IUsage propagate(IEdge edge, IUsageApplied usage) {
		return getPropagate().apply(edge, usage);
	}
}
