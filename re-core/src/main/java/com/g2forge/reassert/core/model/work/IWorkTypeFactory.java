package com.g2forge.reassert.core.model.work;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;

public interface IWorkTypeFactory {
	public IWorkType computeWorkType(ILicenseApplied license);
}
