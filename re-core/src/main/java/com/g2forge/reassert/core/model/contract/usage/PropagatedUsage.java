package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class PropagatedUsage implements IUsage {
	protected final IEdge edge;

	protected final IUsageApplied usage;

	protected final ITerms<IUsageTerm> terms;
}
