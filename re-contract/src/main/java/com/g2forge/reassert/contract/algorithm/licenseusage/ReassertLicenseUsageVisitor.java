package com.g2forge.reassert.contract.algorithm.licenseusage;

import java.util.Collection;
import java.util.stream.Collectors;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.algorithm.visitor.AGraphVisitor;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.MultiLicenseFinding;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.MultiUsageFinding;
import com.g2forge.reassert.core.model.contract.usage.UnspecifiedUsage;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReassertLicenseUsageVisitor extends AGraphVisitor {

	protected final ILicenseUsageAnalyzer licenseUsageAnalyzer;

	@Override
	public void accept(Graph<IVertex, IEdge> graph) {
		final ILicenseUsageAnalyzer licenseUsageAnalyzer = getLicenseUsageAnalyzer();
		for (Artifact<?> artifact : graph.vertexSet().stream().flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance).collect(Collectors.toList())) {
			final IUsageApplied usage = computeUsage(graph, artifact);
			final ILicenseApplied license = computeLicense(graph, artifact);
			licenseUsageAnalyzer.analyze(usage, license, new FindingConsumer(graph, artifact));
		}
	}

	protected ILicenseApplied computeLicense(Graph<IVertex, IEdge> graph, Artifact<?> artifact) {
		final Collection<ILicenseApplied> licenses = HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(ILicenseApplied.class));
		if (licenses.size() == 1) return HCollection.getOne(licenses);

		if (licenses.size() > 1) {
			// Notice: we don't need a finding if no license is specified, because an unspecified license will result in a usage failure
			new FindingConsumer(graph, artifact).found(new MultiLicenseFinding(), licenses);
		}

		return UnspecifiedLicense.create();
	}

	protected IUsageApplied computeUsage(Graph<IVertex, IEdge> graph, Artifact<?> artifact) {
		final Collection<IUsageApplied> usages = HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(IUsageApplied.class));
		if (usages.size() == 1) return HCollection.getOne(usages);

		if (usages.size() > 1) {
			new FindingConsumer(graph, artifact).found(new MultiUsageFinding(), usages);
		}

		return UnspecifiedUsage.create();
	}
}
