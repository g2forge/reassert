package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import com.g2forge.reassert.contract.algorithm.licenseusage.model.LicenseUsageNameScheme;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.contract.model.rule.AContractComparisonRules;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

public abstract class ALicenseUsageRules extends AContractComparisonRules<ILicenseTerm, IUsageTerm> {
	@Override
	protected IContractComparisonScheme<ILicenseTerm, IUsageTerm> getScheme() {
		return LicenseUsageNameScheme.create();
	}
}
