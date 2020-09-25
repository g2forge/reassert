package com.g2forge.reassert.core.model.contract.terms;

import com.g2forge.reassert.core.model.contract.IContractIdentified;

public interface ITermsLoader {
	public <T> ITerms<T> getTerms(IContractIdentified contract, Class<? extends T> termClass);
}
