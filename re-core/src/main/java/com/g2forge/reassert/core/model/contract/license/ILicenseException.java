package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.IContractIdentified;

public interface ILicenseException extends IContractIdentified {
	/**
	 * Test if this exception can be applied to the specified license (or family).
	 * 
	 * @param license The license (or family).
	 * @return {@code true} if this exception can be applied to the specified license (or family).
	 */
	public boolean isApplicable(ILicenseFamily license);
}
