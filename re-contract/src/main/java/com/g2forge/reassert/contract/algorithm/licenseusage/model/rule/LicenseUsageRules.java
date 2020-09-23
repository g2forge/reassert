package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import java.util.Collection;

import com.g2forge.alexandria.java.core.helpers.HCollection;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class LicenseUsageRules implements ILicenseUsageRules {
	@Singular
	protected final Collection<ILicenseUsageRule> rules;

	public LicenseUsageRules(ILicenseUsageRule... rules) {
		this(HCollection.asList(rules));
	}
}