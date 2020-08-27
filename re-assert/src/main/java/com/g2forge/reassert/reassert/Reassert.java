package com.g2forge.reassert.reassert;

import java.util.List;
import java.util.stream.Collectors;

import org.jgrapht.Graph;
import org.jgrapht.graph.AsUnmodifiableGraph;

import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.algorithm.visitor.IGraphVisitor;
import com.g2forge.reassert.core.algorithm.visitor.ReassertGraphExplorer;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IOrigins;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.report.Report;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class Reassert {
	protected final IContext context;

	protected final List<IGraphVisitor> visitors;

	protected Graph<IVertex, IEdge> createGraph(IOrigins origins) {
		return new ReassertGraphExplorer(getContext()).createGraph(origins);
	}

	public IReport createReport(Graph<IVertex, IEdge> graph, IOrigins origins) {
		final Report.ReportBuilder builder = Report.builder().origins(origins);
		if (graph != null) {
			builder.graph(new AsUnmodifiableGraph<>(graph));
			builder.findings(graph.vertexSet().stream().flatMap(ITypeRef.of(IFinding.class)::castIfInstance).collect(Collectors.toList()));
		}
		return builder.build();
	}

	public IReport report(Artifact<?> origin) {
		return report(Origins.builder().origin(origin).build());
	}

	public IReport report(IOrigins origins) {
		final Graph<IVertex, IEdge> graph = createGraph(origins);
		visit(graph);
		return createReport(graph, origins);
	}

	protected void visit(final Graph<IVertex, IEdge> graph) {
		for (IGraphVisitor visitor : getVisitors()) {
			visitor.accept(graph);
		}
	}
}
