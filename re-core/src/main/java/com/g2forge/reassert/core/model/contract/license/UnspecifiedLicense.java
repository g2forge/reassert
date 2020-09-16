package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

/**
 * Indicates that no license was specified.
 */
public class UnspecifiedLicense implements ILicenseSpecific, ISingleton {
	protected static final UnspecifiedLicense INSTANCE = new UnspecifiedLicense();

	public static UnspecifiedLicense create() {
		return INSTANCE;
	}

	private UnspecifiedLicense() {}

	@Override
	public ILicenseFamily getFamily() {
		return null;
	}

	@Override
	public String getName() {
		return "Unspecified license";
	}

	@Override
	public String getShortID() {
		return "UnspecifiedLicense";
	}

	@Override
	public String getSPDXShortID() {
		return null;
	}

	/**
	 * An unspecified license has no terms, which means it has no permissions and thus artifacts with this license cannot cannot be used.
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
