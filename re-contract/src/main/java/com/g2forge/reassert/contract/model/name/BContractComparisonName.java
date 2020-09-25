package com.g2forge.reassert.contract.model.name;

import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class BContractComparisonName implements IContractComparisonName {
	protected final IContractComparisonNameScheme scheme;

	protected final ITerm term;
}
