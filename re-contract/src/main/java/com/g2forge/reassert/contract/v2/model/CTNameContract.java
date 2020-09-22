package com.g2forge.reassert.contract.v2.model;

import com.g2forge.reassert.core.model.contract.IContract;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
public class CTNameContract implements ICTName {
	public CTNameContract(ITerm term, IContract contract) {
		this.term = term;
		this.contract = contract;
		
		if (getContractType() != ContractType.valueOf(contract)) throw new IllegalArgumentException();
	}

	protected final ITerm term;
	
	protected final IContract contract;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final ContractType contractType = ContractType.valueOf(getTerm());
}
