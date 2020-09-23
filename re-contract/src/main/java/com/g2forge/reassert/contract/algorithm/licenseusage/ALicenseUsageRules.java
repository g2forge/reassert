package com.g2forge.reassert.contract.algorithm.licenseusage;

import java.util.Collections;
import java.util.List;

import com.g2forge.reassert.contract.algorithm.licenseusage.model.rule.IRule;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.rule.IRules;

import lombok.Getter;

public abstract class ALicenseUsageRules implements IRules {
	@Getter(lazy = true)
	private final List<IRule> rules = Collections.unmodifiableList(computeRules());

	protected ALicenseUsageRules() {}

	protected abstract List<IRule> computeRules();
}