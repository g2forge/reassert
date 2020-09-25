package com.g2forge.reassert.contract.model.name;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class AContractComparisonName<A extends ITerm> implements IContractComparisonName {
	@JsonIgnore
	protected final IContractComparisonScheme<A, ?> scheme;

	protected final A term;
}
