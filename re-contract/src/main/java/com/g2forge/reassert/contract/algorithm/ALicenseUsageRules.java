package com.g2forge.reassert.contract.algorithm;

import java.util.Collections;
import java.util.List;

import com.g2forge.reassert.contract.model.licenseusage.rule.IRule;
import com.g2forge.reassert.contract.model.licenseusage.rule.IRules;

import lombok.Getter;

public abstract class ALicenseUsageRules implements IRules {
	@Getter(lazy = true)
	private final List<IRule> rules = Collections.unmodifiableList(computeRules());

	protected ALicenseUsageRules() {}

	protected abstract List<IRule> computeRules();
}