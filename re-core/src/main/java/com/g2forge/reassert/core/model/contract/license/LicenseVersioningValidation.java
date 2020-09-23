package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.validate.IValidation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class LicenseVersioningValidation implements IValidation {
	protected final LicenseVersioning versioning;

	protected final ILicenseSpecific license;

	protected final LicenseVersion.Field maxVersionField;

	@Override
	public boolean isValid() {
		final ILicenseSpecific license = getLicense();
		switch (getVersioning()) {
			case Unversioned:
				return getMaxVersionField() == null && license.getVersion() == null && !license.isOrLater();
			case FixedOnly:
				return isValidVersion() && !license.isOrLater();
			case VariableAllowed:
				return isValidVersion();
			default:
				throw new EnumException(LicenseVersioning.class, getVersioning());
		}
	}

	protected boolean isValidVersion() {
		final LicenseVersion version = license.getVersion();
		if (version == null) return true;
		for (int i = getMaxVersionField().ordinal() + 1; i < LicenseVersion.Field.values().length; i++) {
			if (version.get(LicenseVersion.Field.values()[i]) != null) return false;
		}
		return true;
	}
}
