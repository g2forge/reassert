package com.g2forge.reassert.standard.model.contract.usage;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;

import lombok.Getter;

public class StandardUsageDescriber implements IDescriber<StandardUsage>, ISingleton {
	protected static final StandardUsageDescriber INSTANCE = new StandardUsageDescriber();

	public static StandardUsageDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<StandardUsage> type = ITypeRef.of(StandardUsage.class);

	protected StandardUsageDescriber() {}

	@Override
	public IDescription describe(StandardUsage value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return value.name().toLowerCase();
			}

			@Override
			public String getName() {
				return value.getName();
			}
		};
	}
}