package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.helpers.HBinary;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.io.HIO;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.IContractDescriber;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;

import lombok.Getter;

public class UnknownLicenseDescriber implements IContractDescriber<UnknownLicense>, ISingleton {
	protected static final UnknownLicenseDescriber INSTANCE = new UnknownLicenseDescriber();

	public static UnknownLicenseDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<UnknownLicense> type = ITypeRef.of(UnknownLicense.class);

	protected UnknownLicenseDescriber() {}

	@Override
	public IDescription describe(UnknownLicense value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return "unknownlicense-" + HBinary.toHex(HIO.sha1(value.getText()));
			}

			@Override
			public String getName() {
				final String text = value.getText();
				final String line = text.split("\\R", 2)[0];
				return "Unknown License: " + line;
			}
		};
	}
}