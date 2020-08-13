package com.g2forge.reassert.reassert.model.contract;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;

import lombok.Getter;

public class TestUsageDescriber implements IDescriber<TestUsage>, ISingleton {
	protected static final TestUsageDescriber INSTANCE = new TestUsageDescriber();

	public static TestUsageDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<TestUsage> type = ITypeRef.of(TestUsage.class);

	protected TestUsageDescriber() {}

	@Override
	public IDescription describe(TestUsage value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return value.getName().toLowerCase();
			}

			@Override
			public String getName() {
				return value.getName();
			}
		};
	}
}