package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import java.util.Collections;
import java.util.List;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.rule.ILicenseUsageRule.ILicenseUsageRuleBuilder;

import lombok.Getter;

public abstract class ALicenseUsageRules implements ILicenseUsageRules {
	@Getter(lazy = true)
	private final List<ILicenseUsageRule> rules = Collections.unmodifiableList(computeRules());

	protected ALicenseUsageRules() {}

	protected abstract List<ILicenseUsageRule> computeRules();

	protected ILicenseUsageRule rule(final IConsumer1<? super ILicenseUsageRuleBuilder<?, ?>> consumer) {
		return ILicenseUsageRules.rule(consumer);
	}
}