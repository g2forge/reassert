package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.model.contract.IContractIdentified;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

public interface IUsageSpecific extends IUsage, IContractIdentified {
	public static ITerms<IUsageTerm> getTerms(IUsageSpecific usage) {
		return Context.getContext().getTermsLoader().getTerms(usage, IUsageTerm.class);
	}

	@Override
	public String getShortID();

	@Override
	public default String getSPDXShortID() {
		return null;
	}
}
