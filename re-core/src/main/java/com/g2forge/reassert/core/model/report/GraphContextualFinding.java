package com.g2forge.reassert.core.model.report;

import com.g2forge.alexandria.java.adt.name.IStringNamed;
import com.g2forge.reassert.core.model.IVertex;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

/**
 * A finding related to other vertices, generally through {@link com.g2forge.reassert.core.model.contract.Notice} edges.
 */
@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class GraphContextualFinding implements IVertex, IContextualFinding, IStringNamed {
	protected final String name;

	protected final IFinding finding;

	@Override
	public boolean isMaterial() {
		return false;
	}
}
