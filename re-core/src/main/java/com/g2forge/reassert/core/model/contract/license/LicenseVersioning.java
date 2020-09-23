package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.alexandria.java.validate.IValidation;

public enum LicenseVersioning {
	Unversioned,
	FixedOnly,
	VariableAllowed;

	public IValidation validate(ILicenseSpecific license, LicenseVersion.Field maxVersionField) {
		return new LicenseVersioningValidation(this, license, maxVersionField);
	}
}