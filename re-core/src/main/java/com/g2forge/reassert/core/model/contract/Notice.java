package com.g2forge.reassert.core.model.contract;

import org.jgrapht.graph.DefaultEdge;

import com.g2forge.reassert.core.model.IEdge;

import lombok.Builder;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * An edge from an artifact to a notice about it, including {@link IContractApplied contracts} like licenses.
 */
@Getter
@Setter
@ToString(callSuper = false)
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Notice extends DefaultEdge implements IEdge {
	private static final long serialVersionUID = 4456066434633868809L;

	@Override
	public Notice clone() {
		return toBuilder().build();
	}
}
