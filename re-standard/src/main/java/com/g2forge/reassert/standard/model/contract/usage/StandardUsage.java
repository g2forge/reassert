package com.g2forge.reassert.standard.model.contract.usage;

import com.g2forge.reassert.contract.TermsLoader;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardUsage implements IUsage {
	PrivateDistribution,
	OSSLibrary,
	CommercialDistribution,
	CommercialSaaS;

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TermsLoader<StandardUsage, IUsageTerm> loader = new TermsLoader<>(StandardUsage.class, StandardUsageTerm.class);

	@Getter(lazy = true)
	private final ITerms<IUsageTerm> terms = getLoader().getTerms(this);

	@Override
	public String getName() {
		return name() + " usage";
	}
}
