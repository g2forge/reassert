package com.g2forge.reassert.core.algorithm.visitor;

import java.util.Collection;
import java.util.stream.Collectors;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.ILicenseUsageAnalyzer;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.MultiLicenseFinding;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.contract.usage.MultiUsageFinding;
import com.g2forge.reassert.core.model.contract.usage.UnspecifiedUsage;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.work.IWorkType;
import com.g2forge.reassert.core.model.work.Work;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReassertFindingVisitor extends AGraphVisitor {
	protected final ILicenseUsageAnalyzer licenseUsageAnalyzer;

	@Override
	public void accept(Graph<IVertex, IEdge> graph) {
		analyzeLicenseUsage(graph);
		analyzeWorks(graph);
	}

	protected void analyzeLicenseUsage(Graph<IVertex, IEdge> graph) {
		final ILicenseUsageAnalyzer licenseUsageAnalyzer = getLicenseUsageAnalyzer();
		for (Artifact<?> artifact : graph.vertexSet().stream().flatMap(new ATypeRef<Artifact<?>>() {}::castIfInstance).collect(Collectors.toList())) {
			final IUsageApplied usage = computeUsage(graph, artifact);
			final ILicenseApplied license = computeLicense(graph, artifact);
			final IReport report = licenseUsageAnalyzer.report(usage, license);
			found(graph, artifact, report);
		}
	}

	protected void analyzeWorks(Graph<IVertex, IEdge> graph) {
		for (Work work : graph.vertexSet().stream().flatMap(ITypeRef.of(Work.class)::castIfInstance).collect(Collectors.toList())) {
			final IWorkType workType = work.getType();
			final IReport report = workType.report(graph, work);
			found(graph, work, report);
		}
	}

	protected ILicenseApplied computeLicense(Graph<IVertex, IEdge> graph, Artifact<?> artifact) {
		final Collection<ILicenseApplied> licenses = HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(ILicenseApplied.class));
		if (licenses.size() == 1) return HCollection.getOne(licenses);

		if (licenses.size() > 1) {
			// Notice: we don't need a finding if no license is specified, because an unspecified license will result in a usage failure
			final IVertex finding = found(graph, artifact, new MultiLicenseFinding());
			for (ILicenseApplied license : licenses) {
				graph.addEdge(finding, license, new Notice());
			}
		}

		return UnspecifiedLicense.create();
	}

	protected IUsageApplied computeUsage(Graph<IVertex, IEdge> graph, Artifact<?> artifact) {
		final Collection<IUsageApplied> usages = HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(IUsageApplied.class));
		if (usages.size() == 1) return HCollection.getOne(usages);

		if (usages.size() > 1) {
			final IVertex finding = found(graph, artifact, new MultiUsageFinding());
			for (IUsageApplied usage : usages) {
				graph.addEdge(finding, usage, new Notice());
			}
		}

		return UnspecifiedUsage.create();
	}

	protected void found(Graph<IVertex, IEdge> graph, IVertex vertex, final IReport report) {
		for (IFinding finding : report.getFindings()) {
			found(graph, vertex, finding);
		}
	}
}
