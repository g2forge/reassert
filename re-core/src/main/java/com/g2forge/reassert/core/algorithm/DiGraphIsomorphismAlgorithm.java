package com.g2forge.reassert.core.algorithm;

import java.util.Comparator;

import org.jgrapht.Graph;
import org.jgrapht.alg.isomorphism.IsomorphismInspector;
import org.jgrapht.alg.isomorphism.VF2GraphIsomorphismInspector;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class DiGraphIsomorphismAlgorithm<V, E> {
	protected final Comparator<E> EDGE_COMPARATOR = new Comparator<E>() {
		@Override
		public int compare(E o1, E o2) {
			return isEdgeEqual(o1, o2) ? 0 : 1;
		}
	};

	protected final Comparator<V> VERTEX_COMPARATOR = new Comparator<V>() {
		@Override
		public int compare(V o1, V o2) {
			return isVertexEqual(o1, o2) ? 0 : 1;
		}
	};

	protected final Graph<V, E> graph0, graph1;

	@Getter(value = AccessLevel.PROTECTED, lazy = true)
	private final IsomorphismInspector<V, E> inspector = computeInspector(graph0, graph1);

	protected IsomorphismInspector<V, E> computeInspector(Graph<V, E> graph0, Graph<V, E> graph1) {
		return new VF2GraphIsomorphismInspector<>(graph0, graph1, VERTEX_COMPARATOR, EDGE_COMPARATOR);
	}

	protected boolean isEdgeEqual(E e0, E e1) {
		return e0.equals(e1);
	}

	public boolean isMatch() {
		return getInspector().isomorphismExists();
	}

	protected boolean isVertexEqual(V v0, V v1) {
		return v0.equals(v1);
	}
}
