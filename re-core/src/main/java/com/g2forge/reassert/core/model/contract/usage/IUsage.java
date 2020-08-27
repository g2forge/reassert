package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

public interface IUsage extends IUsageApplied, IContractTerms {
	@Override
	public ITerms<IUsageTerm> getTerms();
}
