package com.g2forge.reassert.core.algorithm.visitor;

import java.util.concurrent.atomic.AtomicInteger;

import org.jgrapht.Graph;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.report.GraphContextualFinding;
import com.g2forge.reassert.core.model.report.IFinding;

public abstract class AGraphVisitor implements IGraphVisitor {
	protected final AtomicInteger findingSequence = new AtomicInteger(0);

	protected IVertex found(Graph<IVertex, IEdge> graph, IVertex vertex, final IFinding finding) {
		final String name = finding.getInnermostFinding().getClass().getSimpleName() + " " + findingSequence.getAndIncrement();
		final GraphContextualFinding contextualFinding = new GraphContextualFinding(name, finding);
		graph.addVertex(contextualFinding);
		graph.addEdge(vertex, contextualFinding, new Notice());
		return contextualFinding;
	}
}
