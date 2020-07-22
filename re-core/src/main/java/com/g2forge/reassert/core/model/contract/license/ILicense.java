package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.IContract;
import com.g2forge.reassert.core.model.contract.ITerms;

public interface ILicense extends IContract {
	/**
	 * SPDX license identifier. May be {@code null} when the license is not listed on SPDX.
	 * 
	 * @return The SPDX identifier for this license.
	 * 
	 * @see <a href="https://spdx.org/ids">SPDX IDs</a>
	 */
	public String getSPDX();

	@Override
	public ITerms<ILicenseTerm> getTerms();
}
