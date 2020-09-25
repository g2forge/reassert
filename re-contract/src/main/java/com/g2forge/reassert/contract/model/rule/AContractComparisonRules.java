package com.g2forge.reassert.contract.model.rule;

import java.util.Collections;
import java.util.List;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Getter;

public abstract class AContractComparisonRules<A extends ITerm, B extends ITerm> implements IContractComparisonRules {
	@Getter(lazy = true)
	private final List<IContractComparisonRule> rules = Collections.unmodifiableList(computeRules());

	protected AContractComparisonRules() {}

	protected abstract List<IContractComparisonRule> computeRules();

	protected abstract IContractComparisonScheme<A, B> getScheme();

	protected IContractComparisonRule rule(final IConsumer1<? super IContractComparisonScheme.RuleBuilder<A, B>> consumer) {
		return getScheme().rule(consumer);
	}
}