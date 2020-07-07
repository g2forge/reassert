package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.Terms;

/**
 * Indicates that no license was specified.
 */
public class UnspecifiedLicense implements ILicense, ISingleton {
	protected static final UnspecifiedLicense INSTANCE = new UnspecifiedLicense();

	public static UnspecifiedLicense create() {
		return INSTANCE;
	}

	private UnspecifiedLicense() {}

	@Override
	public String getName() {
		return "Unspecified License";
	}

	@Override
	public String getSPDX() {
		return null;
	}

	/**
	 * An unspecified license has no terms, which means it has no permissions and thus artifacts with this license cannot cannot be used.
	 */
	@Override
	public ITerms<ILicenseTerm> getTerms() {
		return Terms.createNone();
	}
}
