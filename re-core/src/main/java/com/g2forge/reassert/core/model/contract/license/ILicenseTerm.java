package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.terms.ITerm;

public interface ILicenseTerm extends ITerm {
	public enum Type {
		Permission,
		Condition,
		Limitation;
	}

	public Type getType();
}
