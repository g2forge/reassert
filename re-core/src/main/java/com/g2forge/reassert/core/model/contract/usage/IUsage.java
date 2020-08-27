package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.reassert.core.model.contract.IContract;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

public interface IUsage extends IContract {
	@Override
	public ITerms<IUsageTerm> getTerms();
}
