package com.g2forge.reassert.core.model.work;

import org.jgrapht.Graph;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.report.IReport;

public interface IWorkType {
	public default Work create(String name) {
		return new Work(name, this);
	}

	public boolean isIncluded(IEdge edge, boolean outgoing);
	
	public IReport report(Graph<IVertex, IEdge> graph, Work work);
}