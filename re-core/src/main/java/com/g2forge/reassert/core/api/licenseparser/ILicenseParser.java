package com.g2forge.reassert.core.api.licenseparser;

import com.g2forge.reassert.core.model.contract.license.ILicense;

@FunctionalInterface
public interface ILicenseParser {
	public ILicense parse(String text);
}
