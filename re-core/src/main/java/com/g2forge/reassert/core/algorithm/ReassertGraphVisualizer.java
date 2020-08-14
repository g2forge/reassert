package com.g2forge.reassert.core.algorithm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.jgrapht.Graph;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.text.casing.CamelCase;
import com.g2forge.alexandria.java.text.casing.SnakeCase;
import com.g2forge.alexandria.java.type.function.TypeSwitch1.FunctionBuilder;
import com.g2forge.enigma.diagram.dot.convert.DotStringValueQuoteType;
import com.g2forge.enigma.diagram.dot.model.DotEdge;
import com.g2forge.enigma.diagram.dot.model.DotEdgeDirection;
import com.g2forge.enigma.diagram.dot.model.DotGraph;
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
		final IFunction1<IVertex, IDescription> describer = getDescriber();
		final Map<IVertex, DotNode> vertices = new LinkedHashMap<>();
		final List<DotNode> nodes = new ArrayList<>();
		for (IVertex vertex : graph.vertexSet()) {
			nodes.add(vertices.computeIfAbsent(vertex, v -> {
				final IDescription description = describer.apply(v);
				final String name = mangle(description.getIdentifier());
				return DotNode.builder().name(name).label(description.getName()).build();
			}));
		}
		Collections.sort(nodes, new Comparator<DotNode>() {
			@Override
			public int compare(DotNode node0, DotNode node1) {
				return node0.getName().compareTo(node1.getName());
			}
		});

		final List<DotEdge> edges = new ArrayList<>();
		for (IEdge edge : graph.edgeSet()) {
			final DotNode source = vertices.get(graph.getEdgeSource(edge));
			final DotNode target = vertices.get(graph.getEdgeTarget(edge));
			final String label = SnakeCase.SPACE.toString(CamelCase.create().fromString(edge.getClass().getSimpleName()));
			edges.add(DotEdge.builder().node(source.getName()).node(target.getName()).label(label).direction(edge.isDirected() ? null : DotEdgeDirection.NONE).build());
		}
		Collections.sort(edges, new Comparator<DotEdge>() {
			@Override
			public int compare(DotEdge edge0, DotEdge edge1) {
				final String name0 = edge0.getNodes().stream().collect(Collectors.joining(";"));
				final String name1 = edge1.getNodes().stream().collect(Collectors.joining(";"));
				return name0.compareTo(name1);
			}
		});

		return DotGraph.builder().directed(true).name("reassert").statements(HCollection.concatenate(nodes, edges)).build();
	}
}
