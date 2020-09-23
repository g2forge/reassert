package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import java.util.Collections;
import java.util.List;

import lombok.Getter;

public abstract class ALicenseUsageRules implements ILicenseUsageRules {
	@Getter(lazy = true)
	private final List<ILicenseUsageRule> rules = Collections.unmodifiableList(computeRules());

	protected ALicenseUsageRules() {}

	protected abstract List<ILicenseUsageRule> computeRules();
}