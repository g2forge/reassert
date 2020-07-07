package com.g2forge.reassert.core.model.file;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IEdge;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * An edge from a descriptor file (e.g. a pom.xml, package.json, etc) to the artifact it describes.
 */
@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Describes extends DefaultEdge implements IEdge {
	private static final long serialVersionUID = -2161707858251007721L;

	@Override
	public Describes clone() {
		return toBuilder().build();
	}
}
