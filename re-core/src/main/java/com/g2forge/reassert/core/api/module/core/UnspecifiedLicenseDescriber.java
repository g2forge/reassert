package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;

import lombok.Getter;

public class UnspecifiedLicenseDescriber implements IDescriber<UnspecifiedLicense>, ISingleton {
	protected static final UnspecifiedLicenseDescriber INSTANCE = new UnspecifiedLicenseDescriber();

	public static UnspecifiedLicenseDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<UnspecifiedLicense> type = ITypeRef.of(UnspecifiedLicense.class);

	protected UnspecifiedLicenseDescriber() {}

	@Override
	public IDescription describe(UnspecifiedLicense value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return value.getClass().getSimpleName().toLowerCase();
			}

			@Override
			public String getName() {
				return value.getName();
			}
		};
	}
}