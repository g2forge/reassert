package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.reassert.core.model.contract.IContractEnum;

public interface IUsageSpecificEnum extends IUsageSpecific, IContractEnum {
	@Override
	public default String getName() {
		return getShortID() + " usage";
	}

	@Override
	public default String getShortID() {
		return name();
	}
}
