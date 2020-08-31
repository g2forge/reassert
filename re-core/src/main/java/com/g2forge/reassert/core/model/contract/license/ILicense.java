package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

public interface ILicense extends ILicenseApplied, IContractTerms {
	@Override
	public ITerms<ILicenseTerm> getTerms();
}
