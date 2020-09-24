package com.g2forge.reassert.contract.algorithm.licenseusage;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.report.IFindingConsumer;

public interface ILicenseUsageAnalyzer {
	public void analyze(IUsageApplied usage, ILicenseApplied license, IFindingConsumer consumer);
}
