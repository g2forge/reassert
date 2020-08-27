package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Usage implements IUsage {
	protected final String name;

	protected final ITerms<IUsageTerm> terms;
}
