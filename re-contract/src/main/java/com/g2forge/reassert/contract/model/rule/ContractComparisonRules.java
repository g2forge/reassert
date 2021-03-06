package com.g2forge.reassert.contract.model.rule;

import java.util.Collection;

import com.g2forge.alexandria.java.core.helpers.HCollection;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ContractComparisonRules implements IContractComparisonRules {
	@Singular
	protected final Collection<IContractComparisonRule> rules;

	public ContractComparisonRules(IContractComparisonRule... rules) {
		this(HCollection.asList(rules));
	}
}