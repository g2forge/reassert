package com.g2forge.reassert.contract.algorithm.worklicense.model;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

public class WorkLicenseNameScheme implements IContractComparisonScheme<ILicenseTerm, ILicenseTerm>, ISingleton {
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
