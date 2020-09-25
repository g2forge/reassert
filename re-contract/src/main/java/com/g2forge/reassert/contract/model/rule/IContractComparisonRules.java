package com.g2forge.reassert.contract.model.rule;

import java.util.Collection;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.reassert.contract.model.rule.ContractComparisonRule.ContractComparisonRuleBuilder;
import com.g2forge.reassert.contract.model.rule.IContractComparisonRule.IContractComparisonRuleBuilder;

public interface IContractComparisonRules {
	public static IContractComparisonRule rule(final IConsumer1<? super IContractComparisonRuleBuilder<?, ?>> consumer) {
		final ContractComparisonRuleBuilder builder = ContractComparisonRule.builder();
		consumer.accept(builder);
		return builder.build();
	}

	public Collection<IContractComparisonRule> getRules();
}
