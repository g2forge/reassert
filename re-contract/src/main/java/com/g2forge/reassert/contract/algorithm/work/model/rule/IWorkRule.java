package com.g2forge.reassert.contract.algorithm.work.model.rule;

import org.jgrapht.Graph;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.report.IFindingConsumer;
import com.g2forge.reassert.core.model.work.Work;

public interface IWorkRule {
	public boolean isIncluded(IEdge edge, boolean outgoing);

	public void analyze(Graph<IVertex, IEdge> graph, Work work, IFindingConsumer consumer);
}