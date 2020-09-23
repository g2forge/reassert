package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import java.util.Collection;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.rule.ILicenseUsageRule.ILicenseUsageRuleBuilder;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.rule.LicenseUsageRule.LicenseUsageRuleBuilder;

public interface ILicenseUsageRules {
	public static ILicenseUsageRule rule(final IConsumer1<? super ILicenseUsageRuleBuilder<?, ?>> consumer) {
		final LicenseUsageRuleBuilder builder = LicenseUsageRule.builder();
		consumer.accept(builder);
		return builder.build();
	}

	public Collection<ILicenseUsageRule> getRules();
}
