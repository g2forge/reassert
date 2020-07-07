package com.g2forge.reassert.core.model.coordinates;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IEdge;

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
public class Coordinates extends DefaultEdge implements IEdge {
	private static final long serialVersionUID = 7745691971437924717L;

	@Override
	public Coordinates clone() {
		return toBuilder().build();
	}
}
