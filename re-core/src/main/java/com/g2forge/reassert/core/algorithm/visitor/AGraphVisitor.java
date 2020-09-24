package com.g2forge.reassert.core.algorithm.visitor;

import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.report.GraphContextFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IFindingConsumer;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

public abstract class AGraphVisitor implements IGraphVisitor {
	@RequiredArgsConstructor
	@Getter
	protected class FindingConsumer implements IFindingConsumer {
		protected final Graph<IVertex, IEdge> graph;

		protected final Collection<IVertex> vertices;

		public FindingConsumer(Graph<IVertex, IEdge> graph, IVertex... vertices) {
			this(graph, HCollection.asList(vertices));
		}

		@Override
		public void found(Collection<? extends IFinding> findings, Collection<? extends IVertex> vertices) {
			for (IFinding finding : findings) {
				found(finding, vertices);
			}
		}

		@Override
		public IVertex found(IFinding finding, Collection<? extends IVertex> vertices) {
			final String name = finding.getInnermostFinding().getClass().getSimpleName() + " " + findingSequence.getAndIncrement();
			final GraphContextFinding contextualFinding = new GraphContextFinding(name, finding);
			getGraph().addVertex(contextualFinding);
			if (getVertices() != null) for (IVertex vertex : getVertices()) {
				getGraph().addEdge(vertex, contextualFinding, new Notice());
			}
			if (vertices != null) for (IVertex vertex : vertices) {
				getGraph().addEdge(vertex, contextualFinding, new Notice());
			}
			return contextualFinding;
		}
	}

	protected final AtomicInteger findingSequence = new AtomicInteger(0);
}
