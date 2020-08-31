package com.g2forge.reassert.core.model.contract.license;

public interface ILicenseSpecific extends ILicenseFamily {
	public LicenseVersion getVersion();

	public boolean isOrLater();
}
