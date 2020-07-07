package com.g2forge.reassert.core.model.work;

import com.g2forge.reassert.core.model.contract.license.ILicense;

public interface IWorkTypeFactory {
	public IWorkType computeWorkType(ILicense license);
}
