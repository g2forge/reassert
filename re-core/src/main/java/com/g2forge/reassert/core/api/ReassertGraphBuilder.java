package com.g2forge.reassert.core.api;

import java.util.List;

import org.jgrapht.Graph;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class ReassertGraphBuilder implements IReassertGraphBuilder {
	protected final Graph<IVertex, IEdge> graph;

	protected final List<IReassertGraphBuilder.ICallback<?>> callbacks;

	@Override
	public IReassertGraphBuilder callback(IReassertGraphBuilder.ICallback<?> callback) {
		if (callback instanceof IReassertGraphBuilder.RelatedCallback) {
			final IReassertGraphBuilder.RelatedCallback<?> castCallback = (IReassertGraphBuilder.RelatedCallback<?>) callback;
			if (graph.vertexSet().contains(castCallback.getArtifact())) {
				shortcut(castCallback);
				return getThis();
			}
		}
		callbacks.add(callback);
		return getThis();
	}

	@Override
	public ReassertGraphBuilder edge(IVertex source, IVertex target, IEdge edge) {
		getGraph().addEdge(source, target, edge);
		return getThis();
	}

	protected ReassertGraphBuilder getThis() {
		return this;
	}

	protected <Coordinates extends ICoordinates> void shortcut(final IReassertGraphBuilder.RelatedCallback<Coordinates> callback) {
		callback.callback(callback.getArtifact(), this);
	}

	@Override
	public ReassertGraphBuilder vertex(IVertex vertex) {
		getGraph().addVertex(vertex);
		return getThis();
	}
}