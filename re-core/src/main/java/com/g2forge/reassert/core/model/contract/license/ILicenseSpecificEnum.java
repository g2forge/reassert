package com.g2forge.reassert.core.model.contract.license;

public interface ILicenseSpecificEnum extends ILicenseSpecific {
	@Override
	public default String getName() {
		return getShortID().replace('-', ' ') + " License";
	}

	@Override
	public default String getShortID() {
		final String spdxShortID = getSPDXShortID();
		return (spdxShortID == null) ? name() : spdxShortID;
	}

	public String name();
}
