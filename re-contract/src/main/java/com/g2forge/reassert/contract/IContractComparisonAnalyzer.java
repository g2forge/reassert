package com.g2forge.reassert.contract;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.report.IFindingConsumer;

public interface IContractComparisonAnalyzer {
	public void analyze(IUsageApplied usage, ILicenseApplied license, IFindingConsumer consumer);
}
