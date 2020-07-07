package com.g2forge.reassert.core.model;

import com.fasterxml.jackson.annotation.JsonIgnore;

public interface IEdge extends Cloneable {
	public IEdge clone();

	@JsonIgnore
	public default boolean isDirected() {
		return true;
	}

	public default boolean isEdgeTypeEqual(IEdge edge) {
		return this.getClass() == edge.getClass();
	}

	public default int edgeTypeHashCode() {
		return getClass().hashCode();
	}
}
