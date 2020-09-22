package com.g2forge.reassert.contract.v2.model.contract;

import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum TestLicenseTerm implements ILicenseTerm {
	Permission(Type.Permission, "An example permission"),
	Condition(Type.Condition, "An example condition"),
	limitation(Type.Limitation, "An example limitation");

	protected final Type type;

	protected final String description;

	@Override
	public String getName() {
		return name();
	}
}
