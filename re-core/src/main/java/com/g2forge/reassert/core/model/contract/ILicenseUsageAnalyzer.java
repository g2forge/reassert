package com.g2forge.reassert.core.model.contract;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.usage.IUsageApplied;
import com.g2forge.reassert.core.model.report.IReport;

public interface ILicenseUsageAnalyzer {
	public IReport report(IUsageApplied usage, ILicenseApplied license);
}
