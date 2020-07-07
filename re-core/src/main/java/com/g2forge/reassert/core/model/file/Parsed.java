package com.g2forge.reassert.core.model.file;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IEdge;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * An edge from a file to it's parsed contents.
 */
@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Parsed extends DefaultEdge implements IEdge {
	private static final long serialVersionUID = -1158935417835462983L;

	@Override
	public Parsed clone() {
		return toBuilder().build();
	}
}
