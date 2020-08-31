package com.g2forge.reassert.reassert;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.enigma.diagram.dot.convert.DotRenderer;
import com.g2forge.reassert.core.algorithm.ReassertGraphVisualizer;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;

import lombok.AccessLevel;
import lombok.Getter;

public class TestVisualizer implements ISingleton {
	protected static final TestVisualizer INSTANCE = new TestVisualizer();

	public static TestVisualizer create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ReassertGraphVisualizer visualizer = new ReassertGraphVisualizer(Context.getContext());

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final DotRenderer renderer = new DotRenderer();

	protected TestVisualizer() {}

	public String visualize(Graph<IVertex, IEdge> graph) {
		return getRenderer().render(getVisualizer().visualize(graph));
	}
}
