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
public class BSDLicense implements ILicenseSpecific {
	protected final int clauses;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final String SPDXShortID = getFamily().name() + '-' + getClauses() + "-Clause";

	@Getter(lazy = true)
	private final ITerms<ILicenseTerm> terms = StandardLicense.getLoader().getTerms(getShortID());

	@Override
	public StandardLicenseFamily getFamily() {
		return StandardLicenseFamily.BSD;
	}

	@Override
	public String getName() {
		return getShortID().replace('-', ' ') + " license";
	}

	@Override
	public LicenseVersion getVersion() {
		return null;
	}

	@Override
	public boolean isOrLater() {
		return false;
	}

	@Override
	public String toString() {
		return getShortID();
	}
}
