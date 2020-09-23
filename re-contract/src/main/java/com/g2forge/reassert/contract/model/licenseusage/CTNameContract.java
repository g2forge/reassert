package com.g2forge.reassert.contract.model.licenseusage;

import com.g2forge.reassert.core.model.contract.ContractType;
import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Builder;
import lombok.Data;

@Data
@Builder(toBuilder = true)
public class CTNameContract implements ICTName {
	protected final ITerm term;

	protected final IContractTerms contract;

	public CTNameContract(ITerm term, IContractTerms contract) {
		this.term = term;
		this.contract = contract;

		if (ContractType.valueOf(getTerm()) != ContractType.valueOf(contract)) throw new IllegalArgumentException();
	}
}
