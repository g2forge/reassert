package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.IContractEnum;

public interface ILicenseFamilyEnum extends ILicenseFamily, IContractEnum {
	public static <E extends Enum<?> & ILicenseFamily> void validate(Class<E> type) {
		for (E element : type.getEnumConstants()) {
			if (element.getFamily() == null) continue;
			if (!element.isChild(element.getFamily())) throw new Error(String.format("%1$s reports that it belongs to the %2$s family, but it does not report that it is a child of that family!", element.getShortID(), element.getFamily().getShortID()));
		}
	}

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
