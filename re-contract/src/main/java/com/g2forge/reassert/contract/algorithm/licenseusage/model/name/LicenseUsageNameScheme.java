package com.g2forge.reassert.contract.algorithm.licenseusage.model.name;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.model.name.IContractComparisonNameScheme;

public class LicenseUsageNameScheme implements IContractComparisonNameScheme, ISingleton {
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
