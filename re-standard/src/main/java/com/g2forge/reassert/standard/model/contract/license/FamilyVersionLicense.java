package com.g2forge.reassert.standard.model.contract.license;

import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.LicenseVersion;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class FamilyVersionLicense implements ILicenseSpecific {
	protected final StandardLicenseFamily family;

	protected final LicenseVersion version;

	protected final boolean orLater;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final String SPDXShortID = computeString('-');

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = StandardLicense.getLoader().getTerms(getShortID());

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final String name = computeString(' ') + " license";

	protected String computeString(final char separator) {
		final StringBuilder retVal = new StringBuilder();

		final StandardLicenseFamily family = getFamily();
		retVal.append(family.name());

		final StandardLicenseFamily.Versioning versioning = family.getVersioning();
		if (StandardLicenseFamily.Versioning.Unversioned.compareTo(versioning) < 0) {
			retVal.append(separator);
			final LicenseVersion version = getVersion();
			if (version != null) {
				retVal.append(version);
				if (StandardLicenseFamily.Versioning.VariableAllowed.compareTo(versioning) <= 0) {
					retVal.append(separator);
					if (isOrLater()) retVal.append("or").append(separator).append("later");
					else retVal.append("only");
				}
			} else retVal.append("family");
		}

		return retVal.toString();
	}

	@Override
	public String toString() {
		return getShortID();
	}
}
