package com.g2forge.reassert.contract.model.contract;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

public class TestNameScheme implements IContractComparisonScheme<ILicenseTerm, IUsageTerm>, ISingleton {
	protected static final TestNameScheme INSTANCE = new TestNameScheme();

	public static TestNameScheme create() {
		return INSTANCE;
	}

	protected TestNameScheme() {}

	@Override
	public String getAName() {
		return "license";
	}

	@Override
	public String getBName() {
		return "usage";
	}
}
