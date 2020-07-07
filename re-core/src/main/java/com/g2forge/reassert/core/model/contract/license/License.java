package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.reassert.core.model.contract.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class License implements ILicense {
	protected final String name;

	protected final String sPDX;

	protected final ITerms<ILicenseTerm> terms;
}
