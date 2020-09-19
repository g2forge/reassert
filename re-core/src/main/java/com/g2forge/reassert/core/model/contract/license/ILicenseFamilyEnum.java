package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.IContractEnum;

public interface ILicenseFamilyEnum extends ILicenseFamily, IContractEnum {
	@Override
	public default String getName() {
		return getShortID().replace('-', ' ') + " license";
	}

	@Override
	public default String getShortID() {
		final String spdxShortID = getSPDXShortID();
		return (spdxShortID == null) ? name() : spdxShortID;
	}
}
