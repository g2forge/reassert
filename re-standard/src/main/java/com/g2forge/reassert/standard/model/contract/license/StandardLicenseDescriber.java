package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;

import lombok.Getter;

public class StandardLicenseDescriber implements IDescriber<StandardLicense>, ISingleton {
	protected static final StandardLicenseDescriber INSTANCE = new StandardLicenseDescriber();

	public static StandardLicenseDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<StandardLicense> type = ITypeRef.of(StandardLicense.class);

	protected StandardLicenseDescriber() {}

	@Override
	public IDescription describe(StandardLicense value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return value.getShortID();
			}

			@Override
			public String getName() {
				return value.getName();
			}
		};
	}
}