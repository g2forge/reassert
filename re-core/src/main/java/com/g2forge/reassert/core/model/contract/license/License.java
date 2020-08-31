package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class License implements ILicenseSpecific {
	protected final String name;

	protected final String shortID;

	protected final String SPDXShortID;

	protected final ITerms<ILicenseTerm> terms;

	protected final LicenseVersion version;

	protected final boolean orLater;

	@Override
	public ILicenseFamily getFamily() {
		return null;
	}

	@Override
	public boolean isChild(ILicenseFamily license) {
		return false;
	}
}
