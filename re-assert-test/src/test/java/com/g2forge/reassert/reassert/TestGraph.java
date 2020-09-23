package com.g2forge.reassert.reassert;

import java.util.ArrayList;
import java.util.List;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.contract.algorithm.licenseusage.LicenseUsageAnalyzer;
import com.g2forge.reassert.core.algorithm.visitor.IGraphVisitor;
import com.g2forge.reassert.core.algorithm.visitor.ReassertFindingVisitor;
import com.g2forge.reassert.core.algorithm.visitor.ReassertWorkVisitor;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.standard.algorithm.StandardLicenseInheritanceVisitor;
import com.g2forge.reassert.standard.algorithm.StandardLicenseUsageRules;
import com.g2forge.reassert.standard.algorithm.StandardUsageAssignmentVisitor;
import com.g2forge.reassert.standard.algorithm.StandardUsagePropagation;
import com.g2forge.reassert.standard.algorithm.StandardWorkTypeFactory;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class TestGraph {
	public static List<IGraphVisitor> getStandardVisitors() {
		final List<IGraphVisitor> visitors = new ArrayList<>();
		visitors.add(new StandardLicenseInheritanceVisitor());
		visitors.add(new StandardUsageAssignmentVisitor(StandardUsagePropagation.create()));
		visitors.add(new ReassertWorkVisitor(StandardWorkTypeFactory.create()));
		visitors.add(new ReassertFindingVisitor(new LicenseUsageAnalyzer(StandardLicenseUsageRules.create())));
		return visitors;
	}

	@Getter(lazy = true)
	private final IReport report = computeReport();

	protected final Artifact<?> origin;

	protected final List<IGraphVisitor> visitors;

	public TestGraph(Artifact<?> origin, IGraphVisitor... visitors) {
		this(origin, HCollection.asList(visitors));
	}

	protected IReport computeReport() {
		final IContext context = Context.getContext();
		final List<IGraphVisitor> visitors = getVisitors() == null ? getStandardVisitors() : getVisitors();
		return new Reassert(context, visitors).report(getOrigin());
	}

	public Graph<IVertex, IEdge> getGraph() {
		return getReport().getGraph();
	}
}
