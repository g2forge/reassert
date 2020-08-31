package com.g2forge.reassert.core.model.contract;

import com.g2forge.reassert.core.model.contract.terms.ITerms;

public interface IContractTerms extends IContract {
	public ITerms<?> getTerms();
}
