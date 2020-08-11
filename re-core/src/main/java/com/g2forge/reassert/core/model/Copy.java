package com.g2forge.reassert.core.model;

import org.jgrapht.graph.DefaultEdge;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Copy extends DefaultEdge implements IArtifactRelationEdge {
	private static final long serialVersionUID = -1794936358174658546L;

	@Override
	public Copy clone() {
		return toBuilder().build();
	}

	@Override
	@JsonIgnore
	public boolean isDirected() {
		return false;
	}
}
