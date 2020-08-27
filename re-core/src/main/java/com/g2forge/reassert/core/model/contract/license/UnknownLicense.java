package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

/**
 * Represent a license we do not yet recognize, but have some kind of text for.
 */
@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class UnknownLicense implements ILicenseSpecific {
	protected final String text;

	@Override
	public ILicenseFamily getFamily() {
		return null;
	}

	@Override
	public String getName() {
		return "Unknown License";
	}

	@Override
	public String getShortID() {
		return "UnknownLicense";
	}

	@Override
	public String getSPDXShortID() {
		return null;
	}

	/**
	 * An unknown license has no terms, which means it has no permissions and thus artifacts with this license cannot cannot be used.
	 */
	@Override
	public ITerms<ILicenseTerm> getTerms() {
		return Terms.createNone();
	}

	@Override
	public LicenseVersion getVersion() {
		return null;
	}

	@Override
	public boolean isChild(ILicenseFamily license) {
		return false;
	}

	@Override
	public boolean isOrLater() {
		return false;
	}
}
