package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.contract.IContractIdentified;

import lombok.Getter;

public class ContractIdentifiedDescriber implements IDescriber<IContractIdentified>, ISingleton {
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
				return value.getShortID();
			}

			@Override
			public String getName() {
				return value.getName();
			}
		};
	}
}