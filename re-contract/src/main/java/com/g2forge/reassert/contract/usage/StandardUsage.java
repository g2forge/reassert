package com.g2forge.reassert.contract.usage;

import java.util.Map;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.contract.terms.TermsMapper;
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
	private static final Map<StandardUsage, ITerms<IUsageTerm>> allTerms = computeAllTerms();

	protected static Map<StandardUsage, ITerms<IUsageTerm>> computeAllTerms() {
		final Class<StandardUsage> klass = StandardUsage.class;
		return new TermsMapper().<StandardUsage, IUsageTerm>read(klass, StandardUsageTerm.class, new ResourceDataSource(new Resource(klass, klass.getSimpleName().toLowerCase() + ".csv")));
	}

	@Getter(lazy = true)
	private final ITerms<IUsageTerm> terms = computeTerms();

	protected ITerms<IUsageTerm> computeTerms() {
		return getAllTerms().get(this);
	}

	@Override
	public String getName() {
		return name() + " usage";
	}
}
