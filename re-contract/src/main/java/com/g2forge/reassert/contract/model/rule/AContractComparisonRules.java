package com.g2forge.reassert.contract.model.rule;

import java.util.Collections;
import java.util.List;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.reassert.contract.model.rule.IContractComparisonRule.IContractComparisonRuleBuilder;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Getter;

public abstract class AContractComparisonRules implements IContractComparisonRules {
	@Getter(lazy = true)
	private final List<IContractComparisonRule> rules = Collections.unmodifiableList(computeRules());

	protected AContractComparisonRules() {}

	protected abstract List<IContractComparisonRule> computeRules();

	protected IContractComparisonRule rule(final IConsumer1<? super IContractComparisonRuleBuilder<ILicenseTerm, IUsageTerm, ?, ?>> consumer) {
		return IContractComparisonRules.rule(consumer);
	}
}