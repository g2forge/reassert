package com.g2forge.reassert.contract.algorithm.licenseusage.model;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

public class LicenseUsageNameScheme implements IContractComparisonScheme<ILicenseTerm, IUsageTerm>, ISingleton {
	protected static final LicenseUsageNameScheme INSTANCE = new LicenseUsageNameScheme();

	public static LicenseUsageNameScheme create() {
		return INSTANCE;
	}

	protected LicenseUsageNameScheme() {}

	@Override
	public String getAName() {
		return "license";
	}

	@Override
	public String getBName() {
		return "usage";
	}
}
