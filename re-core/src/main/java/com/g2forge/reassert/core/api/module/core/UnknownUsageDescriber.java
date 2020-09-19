package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.helpers.HBinary;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.io.HIO;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.IContractDescriber;
import com.g2forge.reassert.core.model.contract.usage.UnknownUsage;

import lombok.Getter;

public class UnknownUsageDescriber implements IContractDescriber<UnknownUsage>, ISingleton {
	protected static final UnknownUsageDescriber INSTANCE = new UnknownUsageDescriber();

	public static UnknownUsageDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<UnknownUsage> type = ITypeRef.of(UnknownUsage.class);

	protected UnknownUsageDescriber() {}

	@Override
	public IDescription describe(UnknownUsage value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return "unknownusage-" + HBinary.toHex(HIO.sha1(value.getText()));
			}

			@Override
			public String getName() {
				final String text = value.getText();
				final String line = text.split("\\R", 2)[0];
				return "Unknown Usage: " + line;
			}
		};
	}
}