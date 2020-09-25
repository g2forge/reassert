package com.g2forge.reassert.core.model.artifact;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IArtifactRelationEdge;

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
public class Invokes extends DefaultEdge implements IArtifactRelationEdge {
	private static final long serialVersionUID = 7748512374410591396L;

	@Override
	public Invokes clone() {
		return toBuilder().build();
	}
}
