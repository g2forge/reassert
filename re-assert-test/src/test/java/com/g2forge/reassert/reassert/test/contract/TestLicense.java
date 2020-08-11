package com.g2forge.reassert.reassert.test.contract;

import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TestLicense implements ILicense {
	protected final String name;

	protected final String sPDX;

	protected final ITerms<ILicenseTerm> terms;
}
