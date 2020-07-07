package com.g2forge.reassert.core.model.contract;

import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.report.IReport;

public interface ILicenseUsageAnalyzer {
	public IReport report(IUsage usage, ILicense license);
}
