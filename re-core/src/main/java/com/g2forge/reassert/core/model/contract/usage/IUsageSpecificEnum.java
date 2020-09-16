package com.g2forge.reassert.core.model.contract.usage;

public interface IUsageSpecificEnum extends IUsageSpecific {
	@Override
	public default String getName() {
		return getShortID() + " usage";
	}

	@Override
	public default String getShortID() {
		return name();
	}

	public String name();
}
