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
public class Inherits extends DefaultEdge implements IArtifactRelationEdge {
	private static final long serialVersionUID = 8757133431371139986L;
	
	@Override
	public Inherits clone() {
		return toBuilder().build();
	}
}
