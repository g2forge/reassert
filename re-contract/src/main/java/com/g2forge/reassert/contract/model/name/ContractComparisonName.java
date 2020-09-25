package com.g2forge.reassert.contract.model.name;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.core.model.contract.ContractType;
import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Builder;
import lombok.Data;

@Data
@Builder(toBuilder = true)
public class ContractComparisonName implements IContractComparisonName {
	@JsonIgnore
	protected final IContractComparisonScheme<?, ?> scheme;

	protected final ITerm term;

	protected final IContractTerms contract;

	public ContractComparisonName(IContractComparisonScheme<?, ?> scheme, ITerm term, IContractTerms contract) {
		this.scheme = scheme;
		this.term = term;
		this.contract = contract;

		final ContractType termType = ContractType.valueOf(term);
		final ContractType contractType = ContractType.valueOf(contract);
		if (termType != contractType) throw new IllegalArgumentException(String.format("Term %1$S and contract %2$s have different types (%3$s and %4$s respectively)", term, contract, termType, contractType));
	}
}
