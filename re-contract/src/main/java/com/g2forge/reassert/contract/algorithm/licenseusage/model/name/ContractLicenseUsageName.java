package com.g2forge.reassert.contract.algorithm.licenseusage.model.name;

import com.g2forge.reassert.core.model.contract.ContractType;
import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Builder;
import lombok.Data;

@Data
@Builder(toBuilder = true)
public class ContractLicenseUsageName implements ILicenseUsageName {
	protected final ITerm term;

	protected final IContractTerms contract;

	public ContractLicenseUsageName(ITerm term, IContractTerms contract) {
		this.term = term;
		this.contract = contract;

		if (ContractType.valueOf(getTerm()) != ContractType.valueOf(contract)) throw new IllegalArgumentException();
	}
}
