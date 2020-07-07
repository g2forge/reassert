package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.usage.UnspecifiedUsage;

import lombok.Getter;

public class UnspecifiedUsageDescriber implements IDescriber<UnspecifiedUsage>, ISingleton {
	protected static final UnspecifiedUsageDescriber INSTANCE = new UnspecifiedUsageDescriber();

	public static UnspecifiedUsageDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<UnspecifiedUsage> type = ITypeRef.of(UnspecifiedUsage.class);

	protected UnspecifiedUsageDescriber() {}

	@Override
	public IDescription describe(UnspecifiedUsage value) {
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