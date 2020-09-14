package com.g2forge.reassert.standard.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.jgrapht.Graph;
import org.jgrapht.graph.EdgeReversedGraph;
import org.jgrapht.graph.MaskSubgraph;
import org.jgrapht.traverse.TopologicalOrderIterator;

import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.algorithm.visitor.AGraphVisitor;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.core.model.file.Contains;
import com.g2forge.reassert.core.model.file.Describes;
import com.g2forge.reassert.core.model.file.File;
import com.g2forge.reassert.core.model.file.IDescriptor;
import com.g2forge.reassert.core.model.file.Parsed;

@ReassertLegalOpinion
public class StandardLicenseInheritanceVisitor extends AGraphVisitor {
	@Override
	public void accept(Graph<IVertex, IEdge> graph) {
		final List<Artifact<?>> order = HStream.toStream(new TopologicalOrderIterator<>(new EdgeReversedGraph<>(new MaskSubgraph<>(graph, v -> !(v instanceof Artifact), e -> !(e instanceof Inherits))))).map(a -> (Artifact<?>) a).collect(Collectors.toList());

		final Set<ILicenseApplied> removed = new HashSet<>();
		final Map<Artifact<?>, Collection<ILicenseApplied>> licenseMap = new HashMap<>();
		for (Artifact<?> artifact : order) {
			final Map<Boolean, List<Notice>> notices = graph.outgoingEdgesOf(artifact).stream().filter(Notice.class::isInstance).filter(e -> graph.getEdgeTarget(e) instanceof ILicenseApplied).map(Notice.class::cast).collect(Collectors.toList()).stream().collect(Collectors.groupingBy(e -> (graph.getEdgeTarget(e) instanceof UnspecifiedLicense)));
			final List<Notice> specified = notices.get(false);
			// If there is a specified license for this artifact, then we needn't try to find one
			if ((specified != null) && !specified.isEmpty()) {
				licenseMap.computeIfAbsent(artifact, a -> new ArrayList<>()).addAll(specified.stream().map(graph::getEdgeTarget).map(ILicenseApplied.class::cast).collect(Collectors.toList()));
				continue;
			}

			// Find the parents
			final Collection<Artifact<?>> parents = HReassertModel.get(graph, artifact, true, Inherits.class::isInstance, new ATypeRef<Artifact<?>>() {});

			// Make sure all the parents have a specified license
			if (parents.stream().map(licenseMap::get).anyMatch(l -> l == null || l.isEmpty())) continue;

			// Make sure the artifact and parent have exactly one container, and that it's the same
			final Set<Artifact<?>> artifactContainers = getContainers(graph, artifact);
			if (artifactContainers.size() != 1) continue;
			final Set<Artifact<?>> parentContainers = parents.stream().flatMap(parent -> getContainers(graph, parent).stream()).collect(Collectors.toSet());
			if (parentContainers.size() != 1) continue;
			if (!artifactContainers.equals(parentContainers)) continue;

			// Add the newly found licenses
			final Set<ILicenseApplied> licenses = parents.stream().map(licenseMap::get).flatMap(Collection::stream).collect(Collectors.toSet());
			licenseMap.put(artifact, licenses);
			for (ILicenseApplied license : licenses) {
				graph.addEdge(artifact, license, new Notice());
			}

			// Remove any unspecified license edges
			final List<Notice> unspecified = notices.get(true);
			if (unspecified != null) {
				unspecified.stream().map(graph::getEdgeTarget).map(ILicenseApplied.class::cast).forEach(removed::add);
				graph.removeAllEdges(unspecified);
			}
		}

		for (ILicenseApplied license : removed) {
			if (graph.degreeOf(license) == 0) graph.removeVertex(license);
		}
	}

	protected Set<Artifact<?>> getContainers(Graph<IVertex, IEdge> graph, Artifact<?> artifact) {
		final Collection<IDescriptor> descriptors = HReassertModel.get(graph, artifact, false, Describes.class::isInstance, ITypeRef.of(IDescriptor.class));
		final Stream<File> files = descriptors.stream().flatMap(descriptor -> HReassertModel.get(graph, descriptor, false, Parsed.class::isInstance, ITypeRef.of(File.class)).stream());
		return files.flatMap(file -> HReassertModel.get(graph, file, false, Contains.class::isInstance, new ATypeRef<Artifact<?>>() {}).stream()).collect(Collectors.toSet());
	}
}
