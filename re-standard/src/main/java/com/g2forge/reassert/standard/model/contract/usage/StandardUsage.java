package com.g2forge.reassert.standard.model.contract.usage;

import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.usage.IUsageSpecific;
import com.g2forge.reassert.core.model.contract.usage.IUsageSpecificEnum;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardUsage implements IUsageSpecificEnum {
	PrivateDistribution,
	OSSLibrary,
	CommercialDistribution,
	CommercialSaaS;

	@Getter(lazy = true)
	private final ITerms<IUsageTerm> terms = IUsageSpecific.getTerms(this);
}
