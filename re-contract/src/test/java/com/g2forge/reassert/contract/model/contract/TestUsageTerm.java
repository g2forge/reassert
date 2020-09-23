package com.g2forge.reassert.contract.model.contract;

import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum TestUsageTerm implements IUsageTerm {
	Term("An example term");

	protected final String description;

	@Override
	public String getName() {
		return name();
	}
}
