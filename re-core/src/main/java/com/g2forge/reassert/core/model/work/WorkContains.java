package com.g2forge.reassert.core.model.work;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IEdge;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * An edge from a work to an artifact it contains
 */
@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class WorkContains extends DefaultEdge implements IEdge {
	private static final long serialVersionUID = -6037330681878424226L;

	@Override
	public WorkContains clone() {
		return toBuilder().build();
	}
}