package com.g2forge.reassert.reassert.algorithm.example;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.enigma.diagram.dot.convert.DotRenderer;
import com.g2forge.reassert.core.algorithm.ReassertGraphVisualizer;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.reassert.ReassertContext;

import lombok.AccessLevel;
import lombok.Getter;

public class ExampleVisualizer implements ISingleton {
	protected static final ExampleVisualizer INSTANCE = new ExampleVisualizer();

	public static ExampleVisualizer create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ReassertGraphVisualizer visualizer = new ReassertGraphVisualizer(ReassertContext.getContext());

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final DotRenderer renderer = new DotRenderer();

	protected ExampleVisualizer() {}

	public String visualize(Graph<IVertex, IEdge> graph) {
		return getRenderer().render(getVisualizer().visualize(graph));
	}
}
