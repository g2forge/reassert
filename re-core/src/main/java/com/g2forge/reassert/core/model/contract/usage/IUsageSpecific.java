package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.reassert.core.model.contract.IContractIdentified;

public interface IUsageSpecific extends IUsage, IContractIdentified {
	@Override
	public String getShortID();

	@Override
	public default String getSPDXShortID() {
		return null;
	}
}
