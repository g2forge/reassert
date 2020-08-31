package com.g2forge.reassert.core.model.contract;

import com.g2forge.reassert.core.model.IVertex;

public interface IContractApplied extends IVertex, IContract {
	@Override
	public default boolean isMaterial() {
		return false;
	}
}
