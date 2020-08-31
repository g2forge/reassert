package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.IContractDescriber;
import com.g2forge.reassert.core.model.contract.IContractIdentified;

import lombok.Getter;

public class ContractIdentifiedDescriber implements IContractDescriber<IContractIdentified>, ISingleton {
	protected static final ContractIdentifiedDescriber INSTANCE = new ContractIdentifiedDescriber();

	public static ContractIdentifiedDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<IContractIdentified> type = ITypeRef.of(IContractIdentified.class);

	protected ContractIdentifiedDescriber() {}

	@Override
	public IDescription describe(IContractIdentified value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				final String shortID = value.getShortID();
				if (shortID != null) return shortID;
				final String name = value.getName();
				return name == null ? null : name.replaceAll("\\s+", "-").toLowerCase();
			}

			@Override
			public String getName() {
				final String name = value.getName();
				if (name != null) return name;
				return value.getShortID();
			}
		};
	}
}