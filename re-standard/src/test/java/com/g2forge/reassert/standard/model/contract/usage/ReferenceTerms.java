package com.g2forge.reassert.standard.model.contract.usage;

import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum ReferenceTerms implements IUsage {
	// @formatter:off
	PrivateDistribution(Terms.<IUsageTerm>builder().include(StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService, StandardUsageTerm.UseCopy, StandardUsageTerm.DistributingBinary).exclude(StandardUsageTerm.Commercial, StandardUsageTerm.DistributionPublic, StandardUsageTerm.UseLink, StandardUsageTerm.UseModified, StandardUsageTerm.DistributingSource).build()),
	OSSLibrary(Terms.<IUsageTerm>builder().include(StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionPublic, StandardUsageTerm.UseLink, StandardUsageTerm.DistributingBinary, StandardUsageTerm.DistributingSource).exclude(StandardUsageTerm.Commercial, StandardUsageTerm.DistributionService, StandardUsageTerm.UseCopy, StandardUsageTerm.UseModified).build()),
	CommercialDistribution(Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial, StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.UseCopy, StandardUsageTerm.DistributingBinary).exclude(StandardUsageTerm.DistributionService, StandardUsageTerm.UseLink, StandardUsageTerm.UseModified, StandardUsageTerm.DistributingSource).build()),
	CommercialSaaS(Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial, StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService, StandardUsageTerm.UseCopy, StandardUsageTerm.DistributingBinary).exclude(StandardUsageTerm.UseLink, StandardUsageTerm.UseModified, StandardUsageTerm.DistributingSource).build());
	// @formatter:on

	protected final ITerms<IUsageTerm> terms;

	@Override
	public String getName() {
		return name() + " usage";
	}
}
