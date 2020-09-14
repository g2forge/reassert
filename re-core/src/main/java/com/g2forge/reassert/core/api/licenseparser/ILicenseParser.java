package com.g2forge.reassert.core.api.licenseparser;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;

@FunctionalInterface
public interface ILicenseParser {
	public ILicenseApplied parse(String text);
}
