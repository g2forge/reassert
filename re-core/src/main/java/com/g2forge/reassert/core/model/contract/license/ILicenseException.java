package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.IContractIdentified;

public interface ILicenseException extends IContractIdentified {
	public boolean isApplicable(ILicenseFamily license);
}
