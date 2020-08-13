package com.g2forge.reassert.reassert;

import java.util.ArrayList;
import java.util.List;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.license.StandardWorkTypeFactory;
import com.g2forge.reassert.reassert.Reassert;
import com.g2forge.reassert.reassert.ReassertContext;
import com.g2forge.reassert.reassert.algorithm.IGraphVisitor;
import com.g2forge.reassert.reassert.algorithm.ReassertFindingVisitor;
import com.g2forge.reassert.reassert.algorithm.ReassertLicenseVisitor;
import com.g2forge.reassert.reassert.algorithm.ReassertUsageVisitor;
import com.g2forge.reassert.reassert.algorithm.ReassertWorkVisitor;
import com.g2forge.reassert.term.StandardLicenseUsageRules;
import com.g2forge.reassert.term.StandardUsagePropogation;
import com.g2forge.reassert.term.analyze.LicenseUsageAnalyzer;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class TestGraph {
	protected static List<IGraphVisitor> getStandardVisitors() {
		final List<IGraphVisitor> visitors = new ArrayList<>();
		visitors.add(new ReassertLicenseVisitor());
		visitors.add(new ReassertUsageVisitor(StandardUsagePropogation.create()));
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
		final IContext context = ReassertContext.getContext();
		final List<IGraphVisitor> visitors = getVisitors() == null ? getStandardVisitors() : getVisitors();
		return new Reassert(context, visitors).report(getOrigin());
	}

	public Graph<IVertex, IEdge> getGraph() {
		return getReport().getGraph();
	}
}
