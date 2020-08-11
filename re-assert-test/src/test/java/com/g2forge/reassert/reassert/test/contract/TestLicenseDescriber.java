package com.g2forge.reassert.reassert.test.contract;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;

import lombok.Getter;

public class TestLicenseDescriber implements IDescriber<TestLicense>, ISingleton {
	protected static final TestLicenseDescriber INSTANCE = new TestLicenseDescriber();

	public static TestLicenseDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<TestLicense> type = ITypeRef.of(TestLicense.class);

	protected TestLicenseDescriber() {}

	@Override
	public IDescription describe(TestLicense value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return value.getSPDX() == null ? value.getName().toLowerCase() : value.getSPDX();
			}

			@Override
			public String getName() {
				return value.getName();
			}
		};
	}
}