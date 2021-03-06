package com.g2forge.reassert.core.model.work;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IEdge;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * An edge from a work to the license that defined it.
 */
@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class WorkLicense extends DefaultEdge implements IEdge {
	private static final long serialVersionUID = -1474126878142020926L;

	@Override
	public WorkLicense clone() {
		return toBuilder().build();
	}
}