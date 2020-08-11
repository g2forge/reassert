package com.g2forge.reassert.core.model.file;

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
public class Contains extends DefaultEdge implements IArtifactRelationEdge {
	private static final long serialVersionUID = 558526486975433054L;

	@Override
	public Contains clone() {
		return toBuilder().build();
	}
}
