package com.g2forge.reassert.core.algorithm;

import org.jgrapht.Graph;
import org.jgrapht.alg.isomorphism.IsomorphismInspector;
import org.jgrapht.alg.isomorphism.VF2GraphIsomorphismInspector;

import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;

public class ReassertGraphIsomorphism extends DiGraphIsomorphismAlgorithm<IVertex, IEdge> {
	public ReassertGraphIsomorphism(Graph<IVertex, IEdge> graph0, Graph<IVertex, IEdge> graph1) {
		super(graph0, graph1);
	}

	protected Graph<IVertex, IEdge> augment(Graph<IVertex, IEdge> graph0) {
		final Graph<IVertex, IEdge> retVal = HReassertModel.createGraph();
		for (IVertex vertex : graph0.vertexSet()) {
			retVal.addVertex(vertex);
		}
		for (IEdge edge : graph0.edgeSet()) {
			final IVertex source = graph0.getEdgeSource(edge);
			final IVertex target = graph0.getEdgeTarget(edge);
			retVal.addEdge(source, target, edge);
			if (!edge.isDirected()) retVal.addEdge(target, source, edge.clone());
		}
		return retVal;
	}

	protected IsomorphismInspector<IVertex, IEdge> computeInspector(Graph<IVertex, IEdge> graph0, Graph<IVertex, IEdge> graph1) {
		return new VF2GraphIsomorphismInspector<>(augment(graph0), augment(graph1), VERTEX_COMPARATOR, EDGE_COMPARATOR);
	}

	protected boolean isEdgeEqual(IEdge e0, IEdge e1) {
		return e0.getClass() == e1.getClass();
	}
}