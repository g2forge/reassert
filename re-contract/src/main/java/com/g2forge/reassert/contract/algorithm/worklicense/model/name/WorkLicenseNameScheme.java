package com.g2forge.reassert.contract.algorithm.worklicense.model.name;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.model.name.IContractComparisonNameScheme;

public class WorkLicenseNameScheme implements IContractComparisonNameScheme, ISingleton {
	protected static final WorkLicenseNameScheme INSTANCE = new WorkLicenseNameScheme();

	public static WorkLicenseNameScheme create() {
		return INSTANCE;
	}

	protected WorkLicenseNameScheme() {}

	@Override
	public String getAName() {
		return "work license";
	}

	@Override
	public String getBName() {
		return "combined license";
	}
}
