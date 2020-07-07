package com.g2forge.reassert.core.algorithm;

import java.util.HashMap;
import java.util.Map;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.text.casing.CamelCase;
import com.g2forge.alexandria.java.text.casing.SnakeCase;
import com.g2forge.alexandria.java.type.function.TypeSwitch1.FunctionBuilder;
import com.g2forge.enigma.diagram.dot.convert.DotStringValueQuoteType;
import com.g2forge.enigma.diagram.dot.model.DotEdge;
import com.g2forge.enigma.diagram.dot.model.DotEdgeDirection;
import com.g2forge.enigma.diagram.dot.model.DotGraph;
import com.g2forge.enigma.diagram.dot.model.DotGraph.DotGraphBuilder;
import com.g2forge.enigma.diagram.dot.model.DotNode;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;

import lombok.AccessLevel;
import lombok.Getter;

public class ReassertGraphVisualizer {
	@Getter(value = AccessLevel.PROTECTED)
	protected final IFunction1<IVertex, IDescription> describer;

	public ReassertGraphVisualizer(IContext context) {
		describer = new ReassertVertexDescriber(context);
	}

	protected <T> void add(final FunctionBuilder<IVertex, IDescription> builder, IDescriber<T> describer) {
		final Class<T> type = describer.getType().getErasedType();
		builder.add(type, describer::describe);
	}

	protected String mangle(String string) {
		if (string.length() < 1) throw new IllegalArgumentException();
		final String first = string.substring(0, 1).replaceAll("[^" + DotStringValueQuoteType.getCharacterRangeFirst() + "]", "_");
		final String remainder = (string.length() <= 1) ? "" : string.substring(1).replaceAll("[^" + DotStringValueQuoteType.getCharacterRangeRemainder() + "]", "_");
		return first + remainder;
	}

	public DotGraph visualize(Graph<IVertex, IEdge> graph) {
		final DotGraphBuilder retVal = DotGraph.builder().directed(true).name("reassert");

		final IFunction1<IVertex, IDescription> describer = getDescriber();
		final Map<IVertex, DotNode> vertices = new HashMap<>();
		for (IVertex vertex : graph.vertexSet()) {
			retVal.statement(vertices.computeIfAbsent(vertex, v -> {
				final IDescription description = describer.apply(v);
				final String name = mangle(description.getIdentifier());
				return DotNode.builder().name(name).label(description.getName()).build();
			}));
		}

		for (IEdge edge : graph.edgeSet()) {
			final DotNode source = vertices.get(graph.getEdgeSource(edge));
			final DotNode target = vertices.get(graph.getEdgeTarget(edge));
			final String label = SnakeCase.SPACE.toString(CamelCase.create().fromString(edge.getClass().getSimpleName()));
			retVal.statement(DotEdge.builder().node(source.getName()).node(target.getName()).label(label).direction(edge.isDirected() ? null : DotEdgeDirection.NONE).build());
		}

		return retVal.build();
	}
}
